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
	Pid = spawn (rtpsocket, watcher, [self()]),
	io:format ("::: call[~w] thread started.~n", [self()]),
	{ok, {Ip, Pid, []}}.

% handle originate call leg (new media id possibly)
handle_call({message_u, {OrigIp, OrigPort, FromTag, MediaId}}, _From, {Ip, WatcherPid, Parties}) ->
	io:format ("::: call[~w] message [U] OrigIp [~s] OrigPort [~s] MediaId [~s].~n", [self(), OrigIp, OrigPort, MediaId]),
	% search for already  existed
	case lists:keysearch(FromTag, #party.tagfrom, Parties) of
		% call already exists
		{value, Party} ->
			io:format("::: call[~w] Already exists!~n", [self()]),
			{ok, {LocalIp1, LocalPort1}} = inet:sockname(Party#party.fdfrom),
			Reply = " " ++ integer_to_list(LocalPort1) ++ " " ++ inet_parse:ntoa(LocalIp1),
			{reply, Reply, {Ip, WatcherPid, Parties}};
		false ->
			% open new Fd and attach it
			io:format("::: call[~w] Create new socket...~n", [self()]),
			case gen_udp:open(0, [binary, {ip, Ip}, {active, true}]) of
				{ok, Fd} ->
					io:format("::: call[~w] Create new socket... OK~n", [self()]),
					gen_udp:controlling_process(Fd, WatcherPid),

					{ok, Ip} = inet_parse:address(OrigIp),
					Port = list_to_integer(OrigPort),
					{ok, {LocalIp, LocalPort}} = inet:sockname(Fd),

					Party = #party{fdfrom=Fd, origipfrom=Ip, origportfrom=Port, tagfrom=FromTag, mediaidfrom=MediaId},

					Reply = " " ++ integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
					io:format("::: call[~w] answer [~s]~n", [self(), Reply]),
					{reply, Reply, {Ip, WatcherPid, lists:append(Parties, [Party])}};
				{error, Reason} ->
					io:format(" FAILED [~p]~n", [Reason]),
					% FIXME we must answer with error
					{reply, {error, udp_error}, {Ip, WatcherPid, Parties}}
			end
	end;

% handle answered call leg
handle_call({message_l, {OrigIp, OrigPort, FromTag, MediaIdFrom, ToTag, MediaIdTo}}, _From, {Ip, WatcherPid, Parties}) ->
	io:format ("::: call[~w] message [L] OrigIp [~s] OrigPort [~s] MediaIdFrom [~s] MediaIdTo[~s].~n", [self(), OrigIp, OrigPort, MediaIdFrom, MediaIdTo]),
	case lists:keysearch(FromTag, #party.tagfrom, Parties) of
		{value, Party} ->
			io:format("::: call[~w] Already exists!~n", [self()]),
			case gen_udp:open(0, [binary, {ip, Ip}, {active, true}]) of
				{ok, Fd} ->
					io:format(" OK~n"),
					gen_udp:controlling_process(Fd, WatcherPid),
					
					{ok, Ip} = inet_parse:address(OrigIp),
					Port = list_to_integer(OrigPort),
					{ok, {LocalIp, LocalPort}} = inet:sockname(Fd),
					
					NewParty = Party#party{fdto=Fd, origipto=Ip, origportto=Port, tagto=ToTag, mediaidto=MediaIdTo},
					Reply = " " ++ integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
					io:format("::: call[~w] answer [~s]~n", [self(), Reply]),
					List1 = lists:delete(Party, Parties),
					{reply, Reply, {Fd, WatcherPid, lists:append(List1, [NewParty])}};
				{error, Reason} ->
					io:format(" FAILED [~p]~n", [Reason]),
					{reply, {error, udp_error}, {Ip, WatcherPid, Parties}}
			end;
		false ->
			% Call not found.
			% FIXME we must answer with error
			io:format("::: call[~w] ERROR not found~n", [self()]),
			{reply, {error, not_found}, {Ip, WatcherPid, Parties}}
	end;

handle_call(_Other, _From, State) ->
	{noreply, State}.

% rtp from some port
handle_cast({udp, {Fd, Ip, Port, Msg}}, {MainIp, WatcherPid, Parties}) ->	
	case lists:keysearch(Fd, #party.fdfrom, Parties) of
		{value, Party} ->
			if
				Party#party.fdto /= null, Party#party.origipto /= null, Party#party.origportto /= null ->
					gen_udp:send(Party#party.fdto, Party#party.origipto, Party#party.origportto, Msg)
			end,
			if
				Party#party.origipfrom == null, Party#party.origportfrom == null ->
					NewParty = Party#party{origipfrom=Ip, origportfrom=Port},
					List1 = lists:delete(Party, Parties),
					{noreply, {MainIp, WatcherPid, lists:append(List1, [NewParty])}};
				true ->
					{noreply, {MainIp, WatcherPid, Parties}}
			end;
		false ->
			case lists:keysearch(Fd, #party.fdto, Parties) of
				{value, Party} ->
					if
						Party#party.fdfrom /= null, Party#party.origipfrom /= null, Party#party.origportfrom /= null ->
							gen_udp:send(Party#party.fdfrom, Party#party.origipfrom, Party#party.origportfrom, Msg)
					end,
					if
						Party#party.origipto == null, Party#party.origportto == null ->
							NewParty = Party#party{origipto=Ip, origportto=Port},
							List1 = lists:delete(Party, Parties),
							{noreply, {MainIp, WatcherPid, lists:append(List1, [NewParty])}};
						true ->
							{noreply, {MainIp, WatcherPid, Parties}}
					end;
				false ->
					{noreply, {MainIp, WatcherPid, Parties}}
			end
	end;

handle_cast({WatcherPid, timeout}, {MainIp, WatcherPid, Parties}) ->
	{stop, timeout, {MainIp, WatcherPid, Parties}};

% all other messages
handle_cast(_Request, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, {_MainIp, WatcherPid, Parties}) ->
	ok = gen_server:cast({global, rtpproxy}, {call_terminated, {self(), Reason}}),
	WatcherPid ! {self(), destroy},
	lists:foreach(
			fun(X) when X#party.fdfrom /= null, X#party.fdto /= null ->
				gen_udp:close(X#party.fdfrom),
				gen_udp:close(X#party.fdto)
			end,
			Parties),
	io:format("::: call[~w] thread terminated due to reason [~w]~n", [self(), Reason]).

% Fd from which message arrived must be equal to Fd from our state
%handle_info({udp, Fd, Ip, Port, Msg}, State) ->
%	gen_server:cast(self(), {rtp, {Fd, Ip, Port, Msg}}),
%	{noreply, State};

handle_info(Info, State) ->
	io:format("::: call[~w] Info [~w]~n", [self(), Info]),
	{noreply, State}.

