-module(rtpproxy_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
	supervisor:start_link({local, rtpproxy_sup}, rtpproxy_sup, rtpproxy_sup),
	supervisor:start_link({local, media_sup}, rtpproxy_sup, media_sup).

stop(_State) ->
	ok.
