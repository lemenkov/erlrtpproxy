-module(ser_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, StartArgs) ->
	supervisor:start_link({local, ser_sup}, ser_sup, StartArgs).

stop(_State) ->
	ok.
