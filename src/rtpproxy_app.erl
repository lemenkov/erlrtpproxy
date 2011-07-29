-module(rtpproxy_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

-include("../include/common.hrl").

start() ->
	application:start(erlsyslog),

	% Load erlsyslog parameters
	{ok, {SyslogHost, SyslogPort}} = application:get_env(erlsyslog, syslog_address),
	% Replace logger with erlsyslog
	error_logger:add_report_handler(erlsyslog, {0, SyslogHost, SyslogPort}),
	error_logger:tty(false),

	% Start our pool
	pool:start(rtpproxy),
	?INFO("Available node(s) ~p", [pool:get_nodes()]),

%	mnesia:create_schema([node()]),
%	mnesia:start(),
%	mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),
	application:start(rtpproxy).

start(_Type, _StartArgs) ->
	rtpproxy_sup:start_link().

stop(_State) ->
%	mnesia:stop(),
	ok.
