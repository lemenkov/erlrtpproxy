-module(ser_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
	start(normal, []).

start(normal, StartArgs) ->
	application:start(erlsyslog),
	supervisor:start_link({local, ser_sup}, ser_sup, StartArgs).

stop(_State) ->
	ok.
