-module(rtpproxy).

-behaviour(gen_server).
-export([start/1]).
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-export([watcher/0]).

% description of call thread
-record(callthread, {pid=null, callid=null}).

start(Args) ->
	gen_server:start({global, rtpproxy}, rtpproxy, Args, []).

start_link(Args) ->
	gen_server:start_link({global, rtpproxy}, rtpproxy, Args, []).

init([IpAtom, PortAtom]) when is_atom(IpAtom), is_atom(PortAtom) ->
	process_flag(trap_exit, true),
	{ok, Ip} = inet_parse:address(atom_to_list(IpAtom)),
	Port = list_to_integer(atom_to_list(PortAtom)),
	case gen_udp:open(Port, [{ip, Ip}, {active, true}, list]) of
		{ok, Fd} ->
			Pid = spawn(rtpproxy, watcher, []),
			gen_udp:controlling_process(Fd, Pid),
			io:format("RTPProxy started at ~s:~w~n", [inet_parse:ntoa(Ip), Port]),
			{ok, {Fd, Pid, []}};
		{error, Reason} ->
			io:format("RTPPROXY not started. Reason [~p]~n", Reason),
			{stop, Reason}
	end;

init(Args) ->
	io:format ("Some other [~p]", [Args]),
	{stop, "Wrong data"}.

watcher() ->
	receive
		{udp, Fd, Ip, Port, Msg} ->
			case string:tokens(Msg, " ;") of
				[UnixPid, "V"] ->
					io:format("UnixPid [~s], Cmd [V]...", [UnixPid]),
					MsgOut = UnixPid ++ " 20040107\n",
					gen_udp:send(Fd, Ip, Port, [MsgOut]),
					io:format(" OK~n", []);
				[UnixPid, "VF", Params] ->
					io:format("UnixPid [~s], Cmd [VF] Params [~s]...", [UnixPid, Params]),
					MsgOut = UnixPid ++ " 1\n",
					gen_udp:send(Fd, Ip, Port, [MsgOut]),
					io:format(" OK~n", []);
				[UnixPid, "U", CallId, OrigIp, OrigPort, FromTag, MediaId] ->
					Reply = gen_server:call({global, rtpproxy}, {message_u, CallId, OrigIp, OrigPort, FromTag, MediaId}),
					MsgOut = UnixPid ++ Reply,
					gen_udp:send(Fd, Ip, Port, [MsgOut]);
				[UnixPid, "L", CallId, OrigIp, OrigPort, FromTag, MediaIdFrom, ToTag, MediaIdTo] ->
					case gen_server:call({global, rtpproxy}, {message_l, CallId, OrigIp, OrigPort, FromTag, MediaIdFrom, ToTag, MediaIdTo}) of
						{ok, Reply} ->
							MsgOut = UnixPid ++ Reply,
							gen_udp:send(Fd, Ip, Port, [MsgOut]);
						{error, _Error} ->
							io:format ("Critical error!~n")
					end;
				_Other ->
					io:format("Other command [~s]~n", [Msg])
			end,
			watcher();
		{rtpproxy, destroy} ->
			io:format("Watcher: got destroy.~n"),
			ok;
		_Other ->
			io:format("Unhandled call.~n"),
			watcher()
	after 3000 ->
		gen_server:cast({global, rtpproxy}, {timeout, self()}),
		watcher()
	end.

handle_call({message_u, CallId, OrigIp, OrigPort, FromTag, MediaId}, _From, {Fd, Pid, CallsList}) ->
	io:format("Cmd [U] CallId [~s], OrigIp [~s], OrigPort [~s], FromTag [~s] MediaId [~s]~n", [CallId, OrigIp, OrigPort, FromTag, MediaId]),
	case lists:keysearch(CallId, #callthread.callid, CallsList) of
		{value, CallInfo} ->
			%  update existing record
			io:format("Session exists. Updating existing.~n"),
			Reply = gen_server:call(CallInfo#callthread.pid, {message_u, {OrigIp, OrigPort, FromTag, MediaId}}),
			{reply, Reply, {Fd, Pid, CallsList}};
		false ->
			io:format("Session not exists. Creating new:~n"),
			% create new session
			case call:start_link ([]) of
				{ok, CallPid} ->
					io:format(" OK~n"),
					NewCallThread = #callthread{pid=CallPid, callid=CallId},
					Reply = gen_server:call(CallPid, {message_u, {OrigIp, OrigPort, FromTag, MediaId}}),
					{reply, Reply, {Fd, Pid, lists:append (CallsList, [NewCallThread])}};
				Other ->
					io:format ("RTPPROXY: error creating call! [~w]~n", [Other]),
					{reply, {error, cannot_start}, {Fd, Pid, CallsList}}
			end
	end;

handle_call({message_l, CallId, OrigIp, OrigPort, FromTag, MediaIdFrom, ToTag, MediaIdTo}, _From, {Fd, Pid, CallsList}) ->
	io:format("Cmd [L] CallId [~s], OrigIp [~s], OrigPort [~s], FromTag [~s] MediaIdFrom [~s] ToTag [~s] MediaIdTo [~s]~n",	[CallId, OrigIp, OrigPort, FromTag, MediaIdFrom, ToTag, MediaIdTo]),
	case lists:keysearch(CallId, #callthread.callid, CallsList) of
		{value, CallInfo} ->
			% update existing session
			io:format("Session exists. Updating existing.~n"),
			Reply = gen_server:call(CallInfo#callthread.pid, {message_l, {OrigIp, OrigPort, FromTag, MediaIdFrom, ToTag, MediaIdTo}}),
			{reply, {ok, Reply}, {Fd, Pid, CallsList}};
		false ->
			io:format("Session not exists. Do nothing.~n"),
			{reply, {error, not_found}, {Fd, Pid, CallsList}}
	end.

handle_cast({call_terminated, {Pid, _Reason}}, {Fd, Pid, CallsList}) ->
	case lists:keysearch(Pid, #callthread.pid, CallsList) of
		{value, CallThread} ->
			{noreply, {Fd, Pid, lists:delete(CallThread, CallsList)}};
		false ->
			{noreply, {Fd, Pid, CallsList}}
	end;

handle_cast(_Other, State) ->
	{noreply, State}.

handle_info({'EXIT', Pid, _Reason}, {Fd, Pid, CallsList}) ->
	case lists:keysearch(Pid, #callthread.pid, CallsList) of
		{value, CallThread} ->
			io:format("RTPPROXY call [~p] closed~n", [Pid]),
			{noreply, {Fd, Pid, lists:delete(CallThread, CallsList)}};
		false ->
			{noreply, {Fd, Pid, CallsList}}
	end;

handle_info(Info, State) ->
	io:format("RTPPROXY got INFO [~p]~n", [Info]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, {Fd, Pid, _CallsList}) ->
	gen_udp:close(Fd),
	Pid ! {rtpproxy, destroy},
	io:format("RTPPROXY terminated due to reason [~w]~n", [Reason]).

