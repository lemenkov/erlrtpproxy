-module(call).

-behaviour(gen_server).
-export([start/1]).
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-export([watcher/1]).

-record(party, {fdfrom=null, origipfrom=null, origportfrom=null, tagfrom=null, mediaidfrom=null,
		fdto=null,   origipto=null,   origportto=null,   tagto=null,   mediaidto=null}).

start(Args) ->
	gen_server:start(call, Args, []).

start_link(Args) ->
	gen_server:start_link(call, Args, []).

init (_Unused) ->
	process_flag(trap_exit, true),
	{ok, Name} = inet:gethostname(),
	{ok, Ip} = inet:getaddr(Name, inet),
	Pid = spawn (call, watcher, [self()]),
	io:format ("::: call[~w] thread started.~n", [self()]),
	{ok, {Pid, Ip, []}}.

watcher (Parent) ->
	receive
		{udp, Fd, Ip, Port, Msg} ->
			gen_server:cast(Parent, timeout);
%			gen_server:call(Parent, {rtp, {Fd, Ip, Port, Msg}});
		_Other ->
			gen_server:cast(Parent, unhandled)
	after 300 ->
		gen_server:cast(Parent, timeout)
	end,
	watcher(Parent).

% rtp from some port
handle_call({rtp, {Fd, Ip, Port, Msg}}, _From, {Pid, MainIp, Parties}) ->
%	io:format ("::: call[~w] message rtp.~n", [self()]),
	case lists:keysearch(Fd, #party.fdfrom, Parties) of
		{value, Party} ->
%			io:format ("::: call[~w] message 1 rtp. [~p]~n", [self(), Party]),
			if
				Party#party.fdfrom /= null, Party#party.origipfrom /= null, Party#party.origportfrom /= null ->
%					gen_udp:send(Party#party.fdto, Party#party.origipto, Party#party.origportto, [Msg])
					{ok, OrigIp} = inet_parse:address(Party#party.origipfrom),
					gen_udp:send(Party#party.fdfrom, OrigIp, list_to_integer(Party#party.origportfrom), [Msg])
			end,
			if
				Party#party.origipto == null, Party#party.origportto == null ->
					NewParty = Party#party{origipto=Ip, origportto=Port},
					List1 = lists:delete(Party, Parties),
					{reply, ok, {Pid, MainIp, lists:append(List1, [NewParty])}};
				true ->
					{reply, ok, {Pid, MainIp, Parties}}
			end;
		false ->
			case lists:keysearch(Fd, #party.fdto, Parties) of
				{value, Party} ->
%					io:format ("::: call[~w] message 2.0 rtp. [~p]~n", [self(), Party]),
%					if
%						Party#party.fdto /= null, Party#party.origipto /= null, Party#party.origportto /= null ->
%							gen_udp:send(Party#party.fdfrom, Party#party.origipfrom, Party#party.origportfrom, [Msg])
%							io:format ("::: call[~w] message 2.0.1 rtp.~n", [self()]),
							{ok, OrigIp} = inet_parse:address(Party#party.origipto),
%							io:format ("::: call[~w] message 2.0.2 rtp.~n", [self()]),
							gen_udp:send(Party#party.fdto, OrigIp, list_to_integer(Party#party.origportto), [Msg]),
%							io:format ("::: call[~w] message 2.0.3 rtp.~n", [self()])
%					end,
%					io:format ("::: call[~w] message 2.1 rtp.~n", [self()]),
					if
						Party#party.origipfrom == null, Party#party.origportto == null ->
%							io:format ("::: call[~w] message 2.1.1 rtp.~n", [self()]),
							NewParty = Party#party{origipfrom=Ip, origportfrom=Port},
%							io:format ("::: call[~w] message 2.1.2 rtp.~n", [self()]),
							List1 = lists:delete(Party, Parties),
%							io:format ("::: call[~w] message 2.1.3 rtp.~n", [self()]),
							{reply, ok, {Pid, MainIp, lists:append(List1, [NewParty])}};
						true ->
							{reply, ok, {Pid, MainIp, Parties}}
					end;
				false ->
					io:format ("::: call[~w] message UNKNOWN rtp.~n", [self()]),
					{reply, ok, {Pid, MainIp, Parties}}
			end
	end;

% new media id possibly
handle_call({message_u, {OrigIp, OrigPort, FromTag, MediaId}}, _From, {Pid, Ip, Parties}) ->
	% search for already  existed
	io:format ("::: call[~w] message [U] OrigIp [~s] OrigPort [~s] MediaId [~s].~n", [self(), OrigIp, OrigPort, MediaId]),
	case lists:keysearch(FromTag, #party.tagfrom, Parties) of
		{value, Party} ->
			io:format("::: call[~w] Already exists!~n", [self()]),
			{ok, {LocalIp, LocalPort}} = inet:sockname(Party#party.fdfrom),
			Reply = " " ++ integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
			{reply, Reply, {Pid, Ip, Parties}};
		false ->
			% open new Fd and attach it
			io:format("::: call[~w] Create new socket...", [self()]),
			case gen_udp:open(0, [{ip, Ip}, {active, true}]) of
				{ok, Fd} ->
					io:format(" OK~n"),
					gen_udp:controlling_process(Fd, Pid),
					Party = #party{fdfrom=Fd, origipfrom=OrigIp, origportfrom=OrigPort, tagfrom=FromTag, mediaidfrom=MediaId},
%					Party = #party{fdfrom=Fd, tagfrom=FromTag, mediaidfrom=MediaId},
					{ok, {LocalIp, LocalPort}} = inet:sockname(Fd),
					Reply = " " ++ integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
					io:format("::: call[~w] answer [~s]~n", [self(), Reply]),
					{reply, Reply, {Pid, Ip, lists:append(Parties, [Party])}};
				{error, Reason} ->
					io:format(" FAILED [~p]~n", [Reason]),
					{reply, {error, udp_error}, {Pid, Ip, Parties}}
			end
	end;

% handle answer
handle_call({message_l, {OrigIp, OrigPort, FromTag, MediaIdFrom, ToTag, MediaIdTo}}, _From, {Pid, Ip, Parties}) ->
	io:format ("::: call[~w] message [L] OrigIp [~s] OrigPort [~s] MediaIdFrom [~s] MediaIdTo[~s].~n", [self(), OrigIp, OrigPort, MediaIdFrom, MediaIdTo]),
	case lists:keysearch(FromTag, #party.tagfrom, Parties) of
		{value, Party} ->
			io:format("::: call[~w] Already exists!~n", [self()]),
			case gen_udp:open(0, [{ip, Ip}, {active, true}]) of
				{ok, Fd} ->
					io:format(" OK~n"),
					gen_udp:controlling_process(Fd, Pid),
					NewParty = Party#party{fdto=Fd, origipto=OrigIp, origportto=OrigPort, tagto=ToTag, mediaidto=MediaIdTo},
%					NewParty = Party#party{fdto=Fd, tagto=ToTag, mediaidto=MediaIdTo},
					{ok, {LocalIp, LocalPort}} = inet:sockname(Fd),
					Reply = " " ++ integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
					io:format("::: call[~w] answer [~s]~n", [self(), Reply]),
					List1 = lists:delete(Party, Parties),
					{reply, Reply, {Fd, Ip, lists:append(List1, [NewParty])}};
				{error, Reason} ->
					io:format(" FAILED [~p]~n", [Reason]),
					{reply, {error, udp_error}, {Pid, Ip, Parties}}
			end;
		false ->
			% open new Fd and attach it
			io:format("::: call[~w] ERROR not found~n", [self()]),
			{reply, {error, not_found}, {Pid, Ip, Parties}}
	end;

handle_call(_Other, _From, State) ->
	{reply, ok, State}.

% all other messages
handle_cast(timeout, Parties) ->
	io:format ("CALL TIMEOUT~n"),
	exit ({reason, timeout}),
	{noreply, Parties};

handle_cast(_Request, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, {Pid, _MainIp, _Parties}) ->
	_Reply = gen_server:cast(Pid, {call_terminated, {self(), Reason}}),
	io:format("::: call[~w] thread terminated due to reason [~w]~n", [self(), Reason]).

handle_info(Info, State) ->
	io:format("::: call[~w] Info [~w]~n", [self(), Info]),
	{noreply, State}.

