-module(rtpproxy_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
	application:start(erlsyslog),
	application:start(rtpproxy).

start(_Type, _StartArgs) ->
	rtpproxy_sup:start_link().

stop(_State) ->
	ok.
