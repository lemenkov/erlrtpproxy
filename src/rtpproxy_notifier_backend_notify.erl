-module(rtpproxy_notifier_backend_notify).

-behaviour(gen_server).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-record(state, {
	tref,
	notify,
	fd
}).

init([NotifyInfo]) ->
	process_flag(trap_exit, true),
	self() ! {init, NotifyInfo},
	{ok, #state{}}.

handle_call(Call, _From, State) ->
	error_logger:error_msg("SER notify backend: strange call: ~p~n", [Call]),
	{stop, {error, unknown_call}, State}.

handle_cast(Cast, State) ->
	error_logger:error_msg("SER notify backend: strange cast: ~p~n", [Cast]),
	{stop, {error, {unknown_cast, Cast}}, State}.

% Don't send "interim_update" via TCP notification - incompatible with OpenSER
handle_info(interim_update, #state{fd = {gen_tcp, _}} = State) ->
	{noreply, State};
handle_info(interim_update, #state{notify = NotifyInfo, fd = {gen_udp, Fd}} = State) ->
	send(gen_udp, Fd, NotifyInfo),
	{noreply, State};

handle_info({init, NotifyInfo}, _) ->
	{ok, Timeout} = application:get_env(rtpproxy, ttl),
	{ok, TRef} = timer:send_interval(Timeout*1000, interim_update),
	{ok, IgnoreStart} = application:get_env(rtpproxy, ignore_start),
	{Module, Fd} = case application:get_env(rtpproxy, notify_servers) of
		{ok, tcp} ->
			[{addr,{Ip,Port}},{tag,_}] = NotifyInfo,
			{ok, F} = gen_tcp:connect(Ip, Port, [binary, {active, true}]),
			% Don't send "start" via TCP notification - incompatible with OpenSER
			{gen_tcp, F};
		{ok, udp} ->
			{ok, F} = gen_udp:open(0, [binary, {active, true}]),
			IgnoreStart orelse send(gen_udp, F, NotifyInfo),
			{gen_udp, F}
	end,
	error_logger:info_msg("~s: started at ~p.~n", [?MODULE, node()]),
	{noreply, #state{tref = TRef, notify = NotifyInfo, fd = {Module, Fd}}};

handle_info(Info, State) ->
	error_logger:error_msg("SER notify backend: strange info: ~p~n", [Info]),
	{stop, {error, {unknown_info, Info}}, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, #state{tref = TRef, notify = NotifyInfo, fd = {Module, Fd}}) ->
	{ok, IgnoreStop} = application:get_env(rtpproxy, ignore_stop),
	IgnoreStop orelse send(Module, Fd, NotifyInfo),
	{memory, Bytes} = erlang:process_info(self(), memory),
	timer:cancel(TRef),
	Module:close(Fd),
	error_logger:warning_msg("SER notify backend: terminated due to reason [~p] (allocated ~b bytes)", [Reason, Bytes]).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions %%
%%%%%%%%%%%%%%%%%%%%%%%%

send(gen_tcp, Fd, [{addr,{Ip,Port}},{tag,NotifyTag}]) ->
	prim_inet:send(Fd, NotifyTag),
	error_logger:info_msg("SER notify backend: ~w sent to tcp:~s:~b~n", [NotifyTag, inet_parse:ntoa(Ip), Port]);
send(gen_udp, Fd, [{addr,{Ip,Port}},{tag,NotifyTag}]) ->
	prim_inet:sendto(Fd, Ip, Port, NotifyTag),
	error_logger:info_msg("SER notify backend: ~w sent to udp:~s:~b~n", [NotifyTag, inet_parse:ntoa(Ip), Port]).
