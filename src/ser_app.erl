-module(ser_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, StartArgs) ->
	% Load erlsyslog parameters
	application:start(erlsyslog),
	{ok, {SyslogHost, SyslogPort}} = application:get_env(erlsyslog, syslog_address),
	% Replace logger with erlsyslog
	error_logger:add_report_handler(erlsyslog, {0, SyslogHost, SyslogPort}),

	supervisor:start_link({local, ser_sup}, ser_sup, StartArgs).

stop(_State) ->
	ok.
