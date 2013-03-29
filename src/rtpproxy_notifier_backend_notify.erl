-module(rtpproxy_notifier_backend_notify).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
	process_flag(trap_exit, true),
	case application:get_env(rtpproxy, notify_servers) of
		{ok, tcp} ->
			error_logger:info_msg("SER notify backend: ~p - TCP started at ~p~n", [self(), node()]),
			{ok, {tcp, []}};
		{ok, udp} ->
			{ok, Fd} = gen_udp:open(0, [binary, {active, true}]),
			error_logger:info_msg("SER notify backend: ~p - UDP started at ~p~n", [self(), node()]),
			{ok, {udp, Fd}}
	end.

handle_call(Call, _From, State) ->
	error_logger:error_msg("SER notify backend: ~p - strange call: ~p~n", [self(), Call]),
	{reply, {error, unknown_call}, State}.

% Don't send "start" via TCP notification - incompatible with OpenSER
handle_cast({start, _, _, _}, {tcp, _} = State) ->
	{noreply, State};
% Don't send "interim_update" via TCP notification - incompatible with OpenSER
handle_cast({interim_update, _, _, _}, {tcp, _} = State) ->
	{noreply, State};
handle_cast({stop, _, _, [{addr,{Ip,Port}},{tag,NotifyTag}]}, {tcp, FdSet}) ->
	{Fd, NewFdSet} = case proplists:get_value({Ip,Port}, FdSet, null) of
		null ->
			case gen_tcp:connect(Ip, Port, [binary, {active, true}]) of
				{ok, F} -> {F, FdSet ++ [{{Ip,Port}, F}]};
				{error, Err} -> {{error, Err}, FdSet}
			end;
		F ->
			{F, FdSet}
	end,
	case tcp_send(Fd, NotifyTag) of
		ok ->
			error_logger:info_msg("Notification (stop) from ~p sent to tcp:~s:~b~n", [node(), inet_parse:ntoa(Ip), Port]),
			{noreply, {tcp, NewFdSet}};
		{error, E} ->
			error_logger:info_msg("Notification (stop) from ~p CANNOT be sent to tcp:~s:~b due to ~p~n", [node(), inet_parse:ntoa(Ip), Port, E]),
			tcp_close(Fd),
			{noreply, {tcp, lists:delete({{Ip,Port}, Fd}, NewFdSet)}}
	end;

handle_cast({Type, _, _, [{addr,{Ip,Port}},{tag,NotifyTag}]}, {udp, Fd}) ->
	gen_udp:send(Fd, Ip, Port, NotifyTag),
	error_logger:info_msg("Notification (~p) from ~p sent to udp:~s:~b~n", [Type, node(), inet_parse:ntoa(Ip), Port]),
	{noreply, {udp, Fd}};

handle_cast(Cast, State) ->
	error_logger:error_msg("SER notify backend: ~p - strange cast: ~p~n", [self(), Cast]),
	{stop, {error, {unknown_cast, Cast}}, State}.

handle_info({tcp_closed, Fd}, {tcp, FdSet}) ->
	gen_tcp:close(Fd),
	{noreply, {tcp, [Conn || Conn = {{_,_},F} <- FdSet, F /= Fd]}};

handle_info(Info, State) ->
	error_logger:error_msg("SER notify backend: ~p - strange info: ~p~n", [self(), Info]),
	{stop, {error, {unknown_info, Info}}, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, {tcp, FdSet}) ->
	{memory, Bytes} = erlang:process_info(self(), memory),
	lists:map(fun({_Addr,X}) -> gen_tcp:close(X) end, FdSet),
	error_logger:info_msg("SER notify backend: ~p - terminated due to reason [~p] (allocated ~b bytes)", [self(), Reason, Bytes]);
terminate(Reason, {udp, Fd}) ->
	{memory, Bytes} = erlang:process_info(self(), memory),
	gen_udp:close(Fd),
	error_logger:info_msg("SER notify backend: ~p - terminated due to reason [~p] (allocated ~b bytes)", [self(), Reason, Bytes]).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions %%
%%%%%%%%%%%%%%%%%%%%%%%%

tcp_send({error, E}, _) -> {error, E};
tcp_send(Fd, Msg) -> gen_tcp:send(Fd, Msg).

tcp_close({error, _}) -> ok;
tcp_close(Fd) -> gen_tcp:close(Fd).
