-module(ser_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
	application:start(erlsyslog),
	application:start(ser).

start(_Type, _StartArgs) ->
	ser_sup:start_link().

stop(_State) ->
	ok.
