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

-module(tcp_listener).

-behaviour(gen_server).
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	listener,
	acceptor,
	clients = []
}).

-include("common.hrl").

start_link(Args) ->
	gen_server:start_link({local, listener}, ?MODULE, Args, []).

init ([{I0, I1, I2, I3, I4, I5, I6, I7} = IPv6, Port]) when
	is_integer(I0), I0 >= 0, I0 < 65535,
	is_integer(I1), I1 >= 0, I1 < 65535,
	is_integer(I2), I2 >= 0, I2 < 65535,
	is_integer(I3), I3 >= 0, I3 < 65535,
	is_integer(I4), I4 >= 0, I4 < 65535,
	is_integer(I5), I5 >= 0, I5 < 65535,
	is_integer(I6), I6 >= 0, I6 < 65535,
	is_integer(I7), I7 >= 0, I7 < 65535 ->
	process_flag(trap_exit, true),
	Opts = [{ip, IPv6}, list, {packet, raw}, {reuseaddr, true}, {keepalive, true}, {backlog, 30}, {active, false}, inet6],
	{ok, Socket} = gen_tcp:listen(Port, Opts),
	{ok, Ref} = prim_inet:async_accept(Socket, -1),
	error_logger:info_msg("TCP listener started at [~s:~w]~n", [inet_parse:ntoa(IPv6), Port]),
	{ok, #state{listener = Socket, acceptor = Ref}};

init ([{I0, I1, I2, I3} = IPv4, Port]) when
	is_integer(I0), I0 >= 0, I0 < 256,
	is_integer(I1), I1 >= 0, I1 < 256,
	is_integer(I2), I2 >= 0, I2 < 256,
	is_integer(I3), I3 >= 0, I3 < 256 ->
	process_flag(trap_exit, true),
	Opts = [{ip, IPv4}, list, {packet, raw}, {reuseaddr, true}, {keepalive, true}, {backlog, 30}, {active, false}],
	{ok, Socket} = gen_tcp:listen(Port, Opts),
	{ok, Ref} = prim_inet:async_accept(Socket, -1),
	error_logger:info_msg("TCP listener started at [~s:~w]~n", [inet_parse:ntoa(IPv4), Port]),
	{ok, #state{listener = Socket, acceptor = Ref}}.

handle_call(Request, _From, State) ->
	{stop, {unknown_call, Request}, State}.

handle_cast({#cmd{origin = #origin{type = ser, ip = Ip, port = Port}} = Cmd, Answer}, State = #state{clients=Clients}) ->
	Data = ser_proto:encode(Cmd, Answer),
	% Select proper client
	case get_socket(Clients, Ip, Port) of
		error -> ok;
		Fd -> gen_tcp:send(Fd, Data)
	end,
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({tcp, Client, Msg}, State) ->
	inet:setopts(Client, [{active, once}, {packet, raw}, list]),
	{ok, {Ip, Port}} = inet:peername(Client),
	try ser_proto:decode(Msg) of
		#cmd{origin = Origin} = Cmd ->
			gen_server:cast(backend, Cmd#cmd{origin = Origin#origin{ip=Ip, port=Port}})
	catch
		throw:{error_syntax, Error} ->
			error_logger:error_msg("Bad syntax. [~s -> ~s]~n", [Msg, Error]),
			Data = ser_proto:encode({error, syntax, Msg}),
			gen_tcp:send(Client, Data);
		E:C ->
			error_logger:error_msg("Exception. [~s -> ~p:~p]~n", [Msg, E, C]),
			Data = ser_proto:encode({error, syntax, Msg}),
			gen_tcp:send(Client, Data)
	end,
	{noreply, State};

handle_info({tcp_closed, Client}, State = #state{clients=Clients}) ->
	gen_tcp:close(Client),
	error_logger:warning_msg("Client ~p closed connection~n", [Client]),
	{noreply, State#state{clients = proplists:delete(Client, Clients)}};

handle_info({inet_async, ListSock, Ref, {ok, CliSocket}}, #state{listener=ListSock, acceptor=Ref, clients = Clients} = State) ->
	case set_sockopt(ListSock, CliSocket) of
		ok -> ok;
		{error, Reason} -> exit({set_sockopt, Reason})
	end,

	inet:setopts(CliSocket, [{active, once}, {packet, raw}, list]),

	case prim_inet:async_accept(ListSock, -1) of
		{ok, NewRef} -> ok;
		{error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
        end,

	{noreply, State#state{acceptor=NewRef, clients = Clients ++ [CliSocket]}};

handle_info({inet_async, ListSock, Ref, Error}, #state{listener=ListSock, acceptor=Ref} = State) ->
	error_logger:error_msg("Error in socket acceptor: ~p.~n", [Error]),
	{stop, Error, State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(Reason, #state{listener = Listener, clients = Clients}) ->
	gen_tcp:close(Listener),
	lists:map(fun gen_tcp:close/1, Clients),
	error_logger:error_msg("TCP lisener closed: ~p~n", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

set_sockopt(ListSock, CliSocket) ->
	true = inet_db:register_socket(CliSocket, inet_tcp),
	case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
		{ok, Opts} ->
			case prim_inet:setopts(CliSocket, Opts) of
				ok -> ok;
				Error -> gen_tcp:close(CliSocket), Error
			end;
		Error ->
			gen_tcp:close(CliSocket), Error
	end.

get_socket([], _, _) ->
	error;
get_socket([S | Rest], Ip, Port) ->
	case inet:peername(S) of
		{ok, {Ip, Port}} -> S;
		_ -> get_socket(Rest, Ip, Port)
	end.
