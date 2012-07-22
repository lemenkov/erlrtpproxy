-module(rtpproxy_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
	rtpproxy_notifier_sup:start_link(),
	rtpproxy_sup:start_link().

stop(_State) ->
	ok.
