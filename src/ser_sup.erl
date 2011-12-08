-module(ser_sup).
-behaviour(supervisor).

-export([init/1]).

init(Args) ->
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
	{ok, {Type, Addr}} = application:get_env(ser, backend),
	BackendProcess = case Type of
		erlang ->
			{erlang_backend, {erlang_backend, start_link, [Addr]}, permanent, 10000, worker, []};
		proxy ->
			{proxy_backend, {proxy_backend, start_link, [Addr]}, permanent, 10000, worker, []}
	end,

	{ok,{{one_for_one,10,1}, [ListenerProcess, BackendProcess]}}.

