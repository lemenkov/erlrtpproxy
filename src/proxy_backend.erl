%% Copyright (c) 2011 Peter Lemenkov.
%%
%% The MIT License
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%

-module(proxy_backend).
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

-record(state, {fd, ip, port, cmds = []}).

start({I, P}) ->
	gen_server:start({local, backend}, ?MODULE, [I, P], []).
start_link({I, P}) ->
	gen_server:start_link({local, backend}, ?MODULE, [I, P], []).

init ([{I0, I1, I2, I3, I4, I5, I6, I7} = IPv6, Port]) when
	is_integer(I0), 0 =< I0, I0 =< 65535,
	is_integer(I1), 0 =< I1, I1 =< 65535,
	is_integer(I2), 0 =< I2, I2 =< 65535,
	is_integer(I3), 0 =< I3, I3 =< 65535,
	is_integer(I4), 0 =< I4, I4 =< 65535,
	is_integer(I5), 0 =< I5, I5 =< 65535,
	is_integer(I6), 0 =< I6, I6 =< 65535,
	is_integer(I7), 0 =< I7, I7 =< 65535,
	is_integer(Port), 0 =< Port, Port =< 65535 ->
	process_flag(trap_exit, true),
	{ok, Fd} = gen_udp:open(0, [{active, true}, list, inet6]),
	error_logger:info_msg("Proxy backend started.~n"),
	{ok, #state{fd = Fd, ip = IPv6, port = Port}};
init ([{I0, I1, I2, I3} = IPv4, Port]) when
	is_integer(I0), 0 =< I0, I0 =< 255,
	is_integer(I1), 0 =< I1, I1 =< 255,
	is_integer(I2), 0 =< I2, I2 =< 255,
	is_integer(I3), 0 =< I3, I3 =< 255,
	is_integer(Port), 0 =< Port, Port =< 65535 ->
	process_flag(trap_exit, true),
	{ok, Fd} = gen_udp:open(0, [{active, true}, list]),
	error_logger:info_msg("Proxy backend started.~n"),
	{ok, #state{fd = Fd, ip = IPv4, port = Port}}.

handle_call(_Other, _From, State) ->
	{noreply, State}.

handle_cast(#cmd{cookie = Cookie, origin = Origin} = Cmd, #state{fd = Fd, ip = Ip, port = Port, cmds = Cmds} = State) ->
	gen_udp:send(Fd, Ip, Port, ser_proto:encode(Cmd)),
	{noreply, State#state{cmds = [{Cookie, Origin}]}};

handle_cast(stop, State) ->
	{stop, stop, State};

handle_cast(_Request, State) ->
	{noreply, State}.

% Response from rtpproxy
handle_info({udp, Fd, Ip, Port, Msg}, #state{fd = Fd, ip = Ip, port = Port, cmds = Cmds} = State) ->
	try ser_proto:decode(Msg) of
		#response{} = Response ->
			case proplists:get_value(Response#response.cookie, Cmds) of
				undefined ->
					% FIXME shouldn't happened
					{noreply, State};
				Origin ->
					gen_server:cast(listener, Response#response{origin = Origin}),
					{noreply, State#state{cmds = proplists:delete(Response#response.cookie, Cmds)}}
			end
	catch
		throw:{error_syntax, Error} ->
			error_logger:error_msg("Bad syntax. [~s -> ~s]~n", [Msg, Error]),
			Data = ser_proto:encode({error, syntax, Msg}),
			% FIXME
			{noreply, State};
		E:C ->
			error_logger:error_msg("Exception. [~s -> ~p:~p]~n", [Msg, E, C]),
			Data = ser_proto:encode({error, syntax, Msg}),
			% FIXME
			{noreply, State}
	end;

handle_info(Info, State) ->
	error_logger:warning_msg("Info [~p]~n", [Info]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, #state{fd = Fd}) ->
	gen_udp:close(Fd),
	error_logger:error_msg("Proxy backend stopped: ~p~n", [Reason]).
