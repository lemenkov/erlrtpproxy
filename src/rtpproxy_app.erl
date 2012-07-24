-module(rtpproxy_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
	rtpproxy_notifier_sup:start_link(),
	case application:get_env(rtpproxy, backend) of
		{ok, ser} -> ser_sup:start_link();
		_ -> throw({error, notsupported})
	end.

stop(_State) ->
	ok.
