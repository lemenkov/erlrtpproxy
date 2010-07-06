-module(ser_app).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, _StartArgs) ->
	application:start(erlsyslog),
	ser_sup:start_link().

stop(_State) ->
	ok.
