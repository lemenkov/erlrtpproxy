-module(rtpproxy_sup).
-behaviour(supervisor).

-export([init/1]).
-export([start_media/1]).

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
	ListenerProcess = case Proto of
		tcp ->
			?CHILD(tcp_listener, [backend_ser, Ip, Port]);
		udp ->
			?CHILD(udp_listener, [backend_ser, Ip, Port])
	end,

	% Load protocol backend (only SER is supported)
	BackendProcess = case application:get_env(rtpproxy, backend) of
		{ok, ser} -> ?CHILD(backend_ser)
	end,

	% Load storage for mmap-ed files
	StorageProcess = ?CHILD(storage),

	% Load file writer
	FileWriterProcess = ?CHILD(file_writer),

	% Check and load (if configured) notification backends
	RadiusBackendProcess = case application:get_env(rtpproxy, radacct_servers) of
		{ok, _} ->
			[?CHILD(rtpproxy_notifier_backend_radius)];
		_ ->
			[]
	end,
	NotifyBackendProcess = case application:get_env(rtpproxy, notify_servers) of
		{ok, _} ->
			[?CHILD(rtpproxy_notifier_backend_notify)];
		_ ->
			[]
	end,
	NotifyBackends = RadiusBackendProcess ++ NotifyBackendProcess,

	% Load http backend
	{ok, HttpPort} = application:get_env(rtpproxy, http_port),
	%HttpProcess = ?CHILD(mochiweb_http, [[{loop, {http_server, dispatch}}, {port, HttpPort}, {name, http_server}]]),
	HttpProcess = {mochiweb_http, {mochiweb_http, start, [[{loop, {http_server, dispatch}}, {port, HttpPort}, {name, http_server}]]}, transient, 5000, worker, [mochiweb_http]},

	{ok, {SupFlags, [ListenerProcess, BackendProcess, HttpProcess, StorageProcess, FileWriterProcess | NotifyBackends]}}.

start_media(#cmd{callid = C, mediaid = M, from = #party{tag = T}} = Cmd) ->
	Ret = supervisor:start_child(media_sup,
		{
			{media_channel_sup, C, M},
			{supervisor, start_link, [rtpproxy_sup, media_channel_sup]},
			temporary,
			5000,
			supervisor,
			[rtpproxy_sup]
		}
	),
	Pid = case Ret of
		{ok, Child} -> Child;
		{ok, Child, _} -> Child;
		{error,{already_started,Child}} -> Child
	end,
	supervisor:start_child(Pid,
		{{media, C, M, T}, {media, start_link, [Cmd]}, permanent, 5000, worker, [media]}
	).
