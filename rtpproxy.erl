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
			io:format("RTPProxy[~w] started at ~s:~w~n", [self(), inet_parse:ntoa(Ip), Port]),
			{ok, {Fd, []}};
		{error, Reason} ->
			io:format("RTPPROXY not started. Reason [~p]~n", Reason),
			{stop, Reason}
	end;

init(Args) ->
	io:format ("Some other [~p]", [Args]),
	{stop, "Wrong data"}.

handle_call(_Message, _From , State) ->
	{noreply, State}.

handle_cast({call_terminated, {Pid, _Reason}}, {Fd, CallsList}) ->
	case lists:keysearch(Pid, #callthread.pid, CallsList) of
		{value, CallThread} ->
			io:format("RTPPROXY call [~w] closed~n", [Pid]),
			{noreply, {Fd, lists:delete(CallThread, CallsList)}};
		false ->
			{noreply, {Fd, CallsList}}
	end;

handle_cast(_Other, State) ->
	{noreply, State}.

% Call died (due to timeout)
handle_info({'EXIT', Pid, _Reason}, {Fd, CallsList}) ->
	case lists:keysearch(Pid, #callthread.pid, CallsList) of
		{value, CallThread} ->
			io:format("RTPPROXY call [~p] closed~n", [Pid]),
			{noreply, {Fd, lists:delete(CallThread, CallsList)}};
		false ->
			{noreply, {Fd, CallsList}}
	end;

% Fd from which message arrived must be equal to Fd from our state
handle_info({udp, Fd, Ip, Port, Msg}, {Fd, CallsList}) ->
	case string:tokens(Msg, " ;") of
		[UnixPid, "V"] ->
			io:format("UnixPid [~s], Cmd [V]...", [UnixPid]),
			MsgOut = UnixPid ++ " 20040107\n",
			gen_udp:send(Fd, Ip, Port, [MsgOut]),
			io:format(" OK~n", []),
			{noreply, {Fd, CallsList}};
		[UnixPid, "VF", Params] ->
			io:format("UnixPid [~s], Cmd [VF] Params [~s]...", [UnixPid, Params]),
			MsgOut = UnixPid ++ " 1\n",
			gen_udp:send(Fd, Ip, Port, [MsgOut]),
			io:format(" OK~n", []),
			{noreply, {Fd, CallsList}};
		[UnixPid, "U", CallId, OrigIp, OrigPort, FromTag, MediaId] ->
			io:format("Cmd [U] CallId [~s], OrigIp [~s], OrigPort [~s], FromTag [~s] MediaId [~s]~n", [CallId, OrigIp, OrigPort, FromTag, MediaId]),
			case lists:keysearch(CallId, #callthread.callid, CallsList) of
				{value, CallInfo} ->
					%  update existing record
					io:format("Session exists. Updating existing.~n"),
					Reply = gen_server:call(CallInfo#callthread.pid, {message_u, {FromTag, MediaId}}),
					MsgOut = UnixPid ++ Reply,
					gen_udp:send(Fd, Ip, Port, [MsgOut]),
					{noreply, {Fd, CallsList}};
				false ->
					io:format("Session not exists. Creating new:~n"),
					% create new session
					case call:start_link ([]) of
						{ok, CallPid} ->
							io:format(" OK~n"),
							NewCallThread = #callthread{pid=CallPid, callid=CallId},
							Reply = gen_server:call(CallPid, {message_u, {FromTag, MediaId}}),
							MsgOut = UnixPid ++ Reply,
							gen_udp:send(Fd, Ip, Port, [MsgOut]),
							{noreply, {Fd, lists:append (CallsList, [NewCallThread])}};
						Other ->
							io:format ("RTPPROXY: error creating call! [~w]~n", [Other]),
							% FIXME reply to OpenSER with error message
							{noreply, {Fd, CallsList}}
					end
			end;
		[UnixPid, "L", CallId, OrigIp, OrigPort, FromTag, MediaIdFrom, ToTag, MediaIdTo] ->
			io:format("Cmd [L] CallId [~s], OrigIp [~s], OrigPort [~s], FromTag [~s] MediaIdFrom [~s] ToTag [~s] MediaIdTo [~s]~n",	[CallId, OrigIp, OrigPort, FromTag, MediaIdFrom, ToTag, MediaIdTo]),
			case lists:keysearch(CallId, #callthread.callid, CallsList) of
				{value, CallInfo} ->
					% update existing session
		%			io:format("Session exists. Updating existing.~n"),
					Reply = gen_server:call(CallInfo#callthread.pid, {message_l, {FromTag, MediaIdFrom, ToTag, MediaIdTo}}),
					MsgOut = UnixPid ++ Reply,
					gen_udp:send(Fd, Ip, Port, [MsgOut]);
				false ->
					% FIXME reply to OpenSER with error message
					io:format("Session not exists. Do nothing.~n")
			end,
			{noreply, {Fd, CallsList}};
		_Other ->
			io:format("Other command [~s]~n", [Msg]),
			{noreply, {Fd, CallsList}}
	end;

handle_info(Info, State) ->
	io:format("RTPPROXY got INFO [~p]~n", [Info]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, {Fd, _CallsList}) ->
	gen_udp:close(Fd),
	io:format("RTPPROXY terminated due to reason [~w]~n", [Reason]).

