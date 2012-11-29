-module(rtpproxy_notifier).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-record(state, {ignore_start = false, ignore_stop = false}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
	{ok, IgnoreStart} = application:get_env(rtpproxy, ignore_start),
	{ok, IgnoreStop} = application:get_env(rtpproxy, ignore_stop),
	error_logger:info_msg("Started Notifier at ~p~n", [node()]),
	{ok, #state{
			ignore_start = IgnoreStart,
			ignore_stop = IgnoreStop
		}
	}.

handle_call(Message, From, State) ->
	error_logger:warning_msg("Bogus call: ~p from ~p at ~p~n", [Message, From, node()]),
	{reply, {error, unknown_call}, State}.

handle_cast({Type, CallId, MediaId, Addr}, #state{ignore_start = IgnoreStart, ignore_stop = IgnoreStop} = State) when
	Type == start;
	Type == interim_update;
	Type == stop ->
	Send = ((Type == start) and not IgnoreStart) or (Type == interim_update) or ((Type == stop) and not IgnoreStop),
	Send andalso gen_server:cast(rtpproxy_notifier_backend_radius, {Type, CallId, MediaId, Addr}),
	Send andalso gen_server:cast(rtpproxy_notifier_backend_notify, {Type, CallId, MediaId, Addr}),
	{noreply, State};

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(Other, State) ->
	error_logger:warning_msg("Bogus cast: ~p at ~p~n", [Other, node()]),
	{noreply, State}.

handle_info(Other, State) ->
	error_logger:warning_msg("Bogus info: ~p at ~p~n", [Other, node()]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, _State) ->
	gen_server:cast(rtpproxy_notifier_backend_radius, stop),
	gen_server:cast(rtpproxy_notifier_backend_notify, stop),
	error_logger:error_msg("Terminated: ~p at ~p~n", [Reason, node()]),
	ok.
