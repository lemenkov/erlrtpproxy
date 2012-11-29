-module(rtpproxy_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% Helper macros for declaring children of supervisor
-define(CHILD(I), {I, {I, start_link, []}, transient, 5000, worker, [I]}).
-define(CHILD(I,P), {I, {I, start_link, [P]}, transient, 5000, worker, [I]}).

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
			?CHILD(tcp_listener, [backend_ser, Ip, Port]);
		udp ->
			?CHILD(udp_listener, [backend_ser, Ip, Port])
	end,

	% Load protocol backend (only SER is supported)
	BackendProcess = case application:get_env(rtpproxy, backend) of
		{ok, ser} -> ?CHILD(backend_ser, ser)
	end,

	% Load storage for mmap-ed files
	StorageProcess = ?CHILD(storage),

	% Load file writer
	FileWriterProcess = ?CHILD(file_writer),

	% Load notification process
	NotifierProcess = ?CHILD(rtpproxy_notifier),

	{ok, {SupFlags, [ListenerProcess, BackendProcess, StorageProcess, FileWriterProcess, NotifierProcess]}}.

