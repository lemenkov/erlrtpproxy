-module(rtpproxy_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
	application:start(erlsyslog),
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),
	application:start(rtpproxy).

start(_Type, _StartArgs) ->
	rtpproxy_sup:start_link().

stop(_State) ->
	mnesia:stop(),
	ok.
