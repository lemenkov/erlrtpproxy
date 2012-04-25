-module(rtpproxy_notifier).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-record(state, {radius=false, notify=false}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
	RadiusBackend = case application:get_env(?MODULE, radacct_servers) of
		{ok, RadAcctServers} ->
			backend_sup:start_link(radius, RadAcctServers),
			true;
		_ ->
			false
	end,
	NotifyBackend = case application:get_env(?MODULE, notify_servers) of
		{ok, NotifyServers} ->
			backend_sup:start_link(notify, NotifyServers),
			true;
		_ ->
			false
	end,
	error_logger:info_msg("Started Notifier at ~p~n", [node()]),
	{ok, #state{radius = RadiusBackend, notify = NotifyBackend}}.

handle_call(Message, From, State) ->
	error_logger:warning_msg("Bogus call: ~p from ~p at ~p~n", [Message, From, node()]),
	{reply, {error, unknown_call}, State}.

handle_cast({Type, CallId, MediaId, Addr}, #state{radius = RadiusBackend, notify = NotifyBackend} = State) when
	Type == start;
	Type == interim_update;
	Type == stop ->
	case RadiusBackend of
		true -> gen_server:cast(rtpproxy_notifier_backend_radius, {Type, CallId, MediaId, Addr});
		_ -> ok
	end,
	case NotifyBackend of
		true -> gen_server:cast(rtpproxy_notifier_backend_notify, {Type, CallId, MediaId, Addr});
		_ -> ok
	end,
	{noreply, State};

handle_cast(Other, State) ->
	error_logger:warning_msg("Bogus cast: ~p at ~p~n", [Other, node()]),
	{noreply, State}.

handle_info(Other, State) ->
	error_logger:warning_msg("Bogus info: ~p at ~p~n", [Other, node()]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, _State) ->
	error_logger:error_msg("Terminated: ~p at ~p~n", [Reason, node()]),
	ok.
