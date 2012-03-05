-module(rtpproxy_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({global, ?MODULE}, ?MODULE, []).

init([]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 10,
	MaxTimeBetweenRestarts = 1,
	SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetweenRestarts},

	% Load SER listener
	application:load(ser),
	{ok, {Proto, IpStr, Port}} = application:get_env(ser, listen),
	{ok, Ip} = inet_parse:address(IpStr),
	ListenerProcess = case Proto of
		tcp ->
			{tcp_listener, {tcp_listener, start_link, [[Ip, Port]]}, permanent, 10000, worker, []};
		udp ->
			{udp_listener, {udp_listener, start_link, [[Ip, Port]]}, permanent, 10000, worker, []}
	end,

	% Load SER backend
	{ok, Addr} = application:get_env(ser, backend),
	BackendProcess = {backend, {backend, start_link, [Addr]}, permanent, 10000, worker, []},

	% Load main module
	RtpProxy = {rtpproxy,{rtpproxy,start_link,[ignored]},permanent,2000,worker,[rtpproxy]},

	{ok,{SupFlags, [ListenerProcess, BackendProcess, RtpProxy]}}.

