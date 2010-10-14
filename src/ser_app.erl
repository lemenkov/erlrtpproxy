-module(ser_app).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, StartArgs) ->
	application:start(erlsyslog),
	supervisor:start_link({local, ser_sup}, ser_sup, StartArgs).

stop(_State) ->
	ok.
