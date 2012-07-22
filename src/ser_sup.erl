-module(ser_sup).
-behaviour(supervisor).

-export([init/1]).

init(_Args) ->
	% Load listener
	{ok, {Proto, IpStr, Port}} = application:get_env(ser, listen),
	{ok, Ip} = inet_parse:address(IpStr),
	ListenerProcess = case Proto of
		tcp ->
			{tcp_listener, {tcp_listener, start_link, [[Ip, Port]]}, permanent, 10000, worker, []};
		udp ->
			{udp_listener, {udp_listener, start_link, [[Ip, Port]]}, permanent, 10000, worker, []}
	end,

	% Load backend
	{ok, Addr} = application:get_env(ser, backend),
	BackendProcess = {backend, {backend, start_link, [Addr]}, permanent, 10000, worker, []},

	{ok,{{one_for_one,10,1}, [ListenerProcess, BackendProcess]}}.

