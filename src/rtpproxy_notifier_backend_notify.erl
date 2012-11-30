-module(rtpproxy_notifier_backend_notify).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(tcp) ->
	error_logger:info_msg("Started rtpproxy notify protocol backend (TCP) at ~p~n", [node()]),
	{ok, {tcp, []}};
init(udp) ->
	{ok, Fd} = gen_udp:open(0, [binary, {active, true}]),
	error_logger:info_msg("Started rtpproxy notify protocol backend (UDP) at ~p~n", [node()]),
	{ok, {udp, Fd}}.

handle_call(Message, From, State) ->
	error_logger:warning_msg("Bogus call: ~p from ~p at ~p~n", [Message, From, node()]),
	{reply, {error, unknown_call}, State}.

handle_cast({Type, _, _, [{addr,{Ip,Port}},{tag,NotifyTag}]}, {tcp, FdSet}) ->
	% FIXME
	error_logger:info_msg("Message (~p) delivered from ~p to ~s:~b~n", [Type, node(), inet_parse:ntoa(Ip), Port]),
	{noreply, {tcp, FdSet}};
handle_cast({Type, _, _, [{addr,{Ip,Port}},{tag,NotifyTag}]}, {udp, Fd}) ->
	gen_udp:send(Fd, Ip, Port, NotifyTag),
	error_logger:info_msg("Message (~p) delivered from ~p to ~s:~b~n", [Type, node(), inet_parse:ntoa(Ip), Port]),
	{noreply, {udp, Fd}};

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

terminate(Reason, {tcp, FdSet}) ->
	lists:map(fun(X) -> gen_tcp:close(X) end, FdSet),
	error_logger:error_msg("Terminated: ~p at ~p~n", [Reason, node()]),
	ok;
terminate(Reason, {udp, Fd}) ->
	gen_udp:close(Fd),
	error_logger:error_msg("Terminated: ~p at ~p~n", [Reason, node()]),
	ok.
