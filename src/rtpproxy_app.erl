-module(rtpproxy_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
	% Start our pool
	{ok,[[ConfigPath]]} = init:get_argument(config),
	Nodes = pool:start(rtpproxy, " -config " ++ ConfigPath ++ " "),

	application:start(erlsyslog),
	% Load erlsyslog parameters
	{ok, {SyslogHost, SyslogPort}} = application:get_env(erlsyslog, syslog_address),
	% Replace logger with erlsyslog
	rpc:multicall(Nodes, error_logger, add_report_handler, [erlsyslog, {0, SyslogHost, SyslogPort}]),

	% Run RADIUS client on each node
	rpc:multicall(Nodes, application, start, [rtpproxy_radius]),

%	mnesia:create_schema([node()]),
%	mnesia:start(),
%	mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),

	% Load main module
	pool:pspawn(application, start, [rtpproxy]).

start(_Type, _StartArgs) ->
	rtpproxy_sup:start_link().

stop(_State) ->
%	mnesia:stop(),
	ok.
