-module(rtpproxy_sup).
-behaviour(supervisor).

-export([init/1]).

-include("common.hrl").

%% Helper macros for declaring children of supervisor
-define(CHILD(I), {I, {I, start_link, []}, transient, 5000, worker, [I]}).
-define(CHILD(I,P), {I, {I, start_link, [P]}, transient, 5000, worker, [I]}).
-define(CHILD(N,I,P), {N, {I, start_link, [P]}, transient, 5000, worker, [I]}).

init(media_channel_sup) ->
	RestartStrategy = one_for_all,
	MaxRestarts = 0,
	MaxTimeBetweenRestarts = 1, % in seconds
	SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetweenRestarts},

	{ok, {SupFlags, []}};

init(media_sup) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 10,
	MaxTimeBetweenRestarts = 1, % in seconds
	SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetweenRestarts},

	{ok, {SupFlags, []}};

init(rtpproxy_sup) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 10,
	MaxTimeBetweenRestarts = 1,
	SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetweenRestarts},

	% Load generic erlrtpproxy controlling interface listener
	{ok, {Proto, IpStr, Port}} = application:get_env(rtpproxy, listen),
	{ok, Ip} = inet_parse:address(IpStr),

	% Load protocol backend (only SER is supported)
	BackendProcess = case application:get_env(rtpproxy, backend) of
		{ok, ser} -> backend_ser
	end,

	ListenerProcess = case Proto of
		tcp ->
			?CHILD(tcp_listener, [BackendProcess, Ip, Port]);
		udp ->
			?CHILD(udp_listener, [BackendProcess, Ip, Port])
	end,

	% Load storage for mmap-ed files
	StorageProcess = ?CHILD(storage),

	% Load http backend
	HttpProcess = case application:get_env(rtpproxy, http_port) of
		{ok, HttpPort} ->
			%HttpProcess = ?CHILD(mochiweb_http, [[{loop, {http_server, dispatch}}, {port, HttpPort}, {name, http_server}]]),
			[{mochiweb_http, {mochiweb_http, start, [[{loop, {http_server, dispatch}}, {port, HttpPort}, {name, http_server}]]}, transient, 5000, worker, [mochiweb_http]}];
		_ -> []
	end,

	SentinelProcess = ?CHILD(sentinel),

	OffloaderRtpProcess = ?CHILD(offloader_rtp),

	Children = [ListenerProcess, SentinelProcess, OffloaderRtpProcess] ++ HttpProcess ++ [StorageProcess],

	{ok, {SupFlags, Children}}.
