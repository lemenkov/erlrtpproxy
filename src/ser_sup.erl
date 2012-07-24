-module(ser_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link(?MODULE, []).

init([]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 10,
	MaxTimeBetweenRestarts = 1,
	SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetweenRestarts},

	% Load generic erlrtpproxy controlling interface listener
	{ok, {Proto, IpStr, Port}} = application:get_env(rtpproxy, listen),
	{ok, Ip} = inet_parse:address(IpStr),
	ListenerProcess = case Proto of
		tcp ->
			{tcp_listener, {tcp_listener, start_link, [[backend_ser, Ip, Port]]}, transient, 10000, worker, []};
		udp ->
			{udp_listener, {udp_listener, start_link, [[backend_ser, Ip, Port]]}, transient, 10000, worker, []}
	end,

	% Load SER protocol backend
	BackendProcess = {backend_ser, {backend_ser, start_link, [ser]}, transient, 10000, worker, []},

	{ok,{SupFlags, [ListenerProcess, BackendProcess]}}.

