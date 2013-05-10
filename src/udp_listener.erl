%%%----------------------------------------------------------------------
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 3 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(udp_listener).
-author('lemenkov@gmail.com').

-behaviour(gen_server).
-export([start/1]).
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include("common.hrl").

start(Args) ->
	gen_server:start({local, listener}, ?MODULE, Args, []).
start_link(Args) ->
	gen_server:start_link({local, listener}, ?MODULE, Args, []).

init ([Backend, {I0, I1, I2, I3, I4, I5, I6, I7} = IPv6, Port]) when
	is_integer(I0), I0 >= 0, I0 < 65535,
	is_integer(I1), I1 >= 0, I1 < 65535,
	is_integer(I2), I2 >= 0, I2 < 65535,
	is_integer(I3), I3 >= 0, I3 < 65535,
	is_integer(I4), I4 >= 0, I4 < 65535,
	is_integer(I5), I5 >= 0, I5 < 65535,
	is_integer(I6), I6 >= 0, I6 < 65535,
	is_integer(I7), I7 >= 0, I7 < 65535 ->
	process_flag(trap_exit, true),
	{ok, Fd} = gen_udp:open(Port, [{ip, IPv6}, {active, true}, binary, inet6]),
	error_logger:info_msg("UDP listener: started at [~s:~w]~n", [inet_parse:ntoa(IPv6), Port]),
	{ok, {Backend, Fd}};
init ([Backend, {I0, I1, I2, I3} = IPv4, Port]) when
	is_integer(I0), I0 >= 0, I0 < 256,
	is_integer(I1), I1 >= 0, I1 < 256,
	is_integer(I2), I2 >= 0, I2 < 256,
	is_integer(I3), I3 >= 0, I3 < 256 ->
	process_flag(trap_exit, true),
	{ok, Fd} = gen_udp:open(Port, [{ip, IPv4}, {active, true}, binary]),
	error_logger:info_msg("UDP listener: started at [~s:~w]~n", [inet_parse:ntoa(IPv4), Port]),
	{ok, {Backend, Fd}}.

handle_call(Call, _From, State) ->
	error_logger:error_msg("UDP listener: strange call: ~p~n", [Call]),
	{stop, {error, {unknown_call, Call}}, State}.

handle_cast({reply, Cmd, Reply}, {Backend, Fd}) ->
	{Msg, Ip, Port} = Backend:reply(Cmd, Reply),
	prim_inet:sendto(Fd, Ip, Port, Msg),
	error_logger:error_msg("UDP listener: reply ~s sent to ~s:~b (elapsed time: ~b microsec)~n", [Msg, inet_parse:ntoa(Ip), Port, timer:now_diff(os:timestamp(), Cmd#cmd.timestamp)]),
	{noreply, {Backend, Fd}};

handle_cast(Cast, State) ->
	error_logger:error_msg("UDP listener: strange cast: ~p~n", [Cast]),
	{stop, {error, {unknown_cast, Cast}}, State}.

% Fd from which message arrived must be equal to Fd from our state
handle_info({udp, Fd, Ip, Port, Msg}, {Backend, Fd}) ->
	Begin = os:timestamp(),
	error_logger:error_msg("UDP listener: command ~s recv from ~s:~b~n", [Msg, inet_parse:ntoa(Ip), Port]),
	case Backend:command(Msg, Ip, Port, Begin) of
		{Data, Ip, Port} ->
			prim_inet:sendto(Fd, Ip, Port, Data),
			error_logger:error_msg("UDP listener: reply ~s sent to ~s:~b (elapsed time: ~b microsec)~n", [Data, inet_parse:ntoa(Ip), Port, timer:now_diff(os:timestamp(), Begin)]);
		_ -> ok
	end,
	{noreply, {Backend, Fd}};

handle_info(Info, State) ->
	error_logger:error_msg("UDP listener: strange info: ~p~n", [Info]),
	{stop, {error, {unknown_info, Info}}, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, {_, Fd}) ->
	{memory, Bytes} = erlang:process_info(self(), memory),
	gen_udp:close(Fd),
	error_logger:info_msg("UDP listener: terminated due to reason: ~p (allocated ~b bytes)", [Reason, Bytes]).
