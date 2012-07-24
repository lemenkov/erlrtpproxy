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
-compile({parse_transform, do}).

-export([start/1]).
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	parent,
	listener,
	acceptor,
	clients = []
}).

-include("../include/common.hrl").

start(Args) ->
	gen_server:start({local, listener}, ?MODULE, Args, []).
start_link(Args) ->
	gen_server:start_link({local, listener}, ?MODULE, Args, []).

init ([Parent, {I0, I1, I2, I3, I4, I5, I6, I7} = IPv6, Port]) when
	is_integer(I0), I0 >= 0, I0 < 65535,
	is_integer(I1), I1 >= 0, I1 < 65535,
	is_integer(I2), I2 >= 0, I2 < 65535,
	is_integer(I3), I3 >= 0, I3 < 65535,
	is_integer(I4), I4 >= 0, I4 < 65535,
	is_integer(I5), I5 >= 0, I5 < 65535,
	is_integer(I6), I6 >= 0, I6 < 65535,
	is_integer(I7), I7 >= 0, I7 < 65535 ->
	Opts = [{ip, IPv6}, binary, {packet, line}, {reuseaddr, true}, {keepalive, true}, {backlog, 30}, {active, false}, inet6],
	{ok, Socket} = gen_tcp:listen(Port, Opts),
	{ok, Ref} = prim_inet:async_accept(Socket, -1),
	error_logger:info_msg("TCP listener started at [~s:~w]~n", [inet_parse:ntoa(IPv6), Port]),
	{ok, #state{parent = Parent, listener = Socket, acceptor = Ref}};

init ([Parent, {I0, I1, I2, I3} = IPv4, Port]) when
	is_integer(I0), I0 >= 0, I0 < 256,
	is_integer(I1), I1 >= 0, I1 < 256,
	is_integer(I2), I2 >= 0, I2 < 256,
	is_integer(I3), I3 >= 0, I3 < 256 ->
	Opts = [{ip, IPv4}, binary, {packet, line}, {reuseaddr, true}, {keepalive, true}, {backlog, 30}, {active, false}],
	{ok, Socket} = gen_tcp:listen(Port, Opts),
	{ok, Ref} = prim_inet:async_accept(Socket, -1),
	error_logger:info_msg("TCP listener started at [~s:~w]~n", [inet_parse:ntoa(IPv4), Port]),
	{ok, #state{parent = Parent, listener = Socket, acceptor = Ref}}.

handle_call(Other, _From, State) ->
	error_logger:warning_msg("TCP listener: strange call: ~p~n", [Other]),
	{noreply, State}.

handle_cast({msg, Msg, Ip, Port}, State = #state{clients=Clients}) ->
	% Select proper client
	case get_socket(Clients, Ip, Port) of
		error -> ok;
		Fd -> gen_tcp:send(Fd, Msg)
	end,
	{noreply, State};

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(Other, State) ->
	error_logger:warning_msg("TCP listener: strange cast: ~p~n", [Other]),
	{noreply, State}.

handle_info({tcp, Client, Msg}, #state{parent = Parent} = State) ->
	inet:setopts(Client, [{active, once}, {packet, line}, binary]),
	{ok, {Ip, Port}} = inet:peername(Client),
	gen_server:cast(Parent, {msg, Msg, Ip, Port}),
	{noreply, State};

handle_info({tcp_closed, Client}, State = #state{clients=Clients}) ->
	gen_tcp:close(Client),
	error_logger:warning_msg("Client ~p closed connection~n", [Client]),
	{noreply, State#state{clients = lists:delete(Client, Clients)}};

handle_info({inet_async, ListSock, Ref, {ok, CliSocket}}, #state{listener=ListSock, acceptor=Ref, clients = Clients} = State) ->
	case set_sockopt(ListSock, CliSocket) of
		ok -> ok;
		{error, Reason} -> exit({set_sockopt, Reason})
	end,

	inet:setopts(CliSocket, [{active, once}, {packet, line}, binary]),

	case prim_inet:async_accept(ListSock, -1) of
		{ok, NewRef} -> ok;
		{error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
        end,

	{noreply, State#state{acceptor=NewRef, clients = Clients ++ [CliSocket]}};

handle_info({inet_async, ListSock, Ref, Error}, #state{listener=ListSock, acceptor=Ref} = State) ->
	error_logger:error_msg("Error in socket acceptor: ~p.~n", [Error]),
	{stop, Error, State};

handle_info(Info, State) ->
	error_logger:warning_msg("TCP listener: strange info: ~p~n", [Info]),
	{noreply, State}.

terminate(Reason, #state{listener = Listener, clients = Clients}) ->
	gen_tcp:close(Listener),
	lists:map(fun gen_tcp:close/1, Clients),
	error_logger:error_msg("TCP listener closed: ~p~n", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

set_sockopt(ListSock, CliSocket) ->
	true = inet_db:register_socket(CliSocket, inet_tcp),
	case do([error_m ||
			Opts <- prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]),
			prim_inet:setopts(CliSocket, Opts)])
	of
		ok -> ok;
		Error -> gen_tcp:close(CliSocket), Error
	end.

get_socket([], _, _) ->
	error;
get_socket([S | Rest], Ip, Port) ->
	case inet:peername(S) of
		{ok, {Ip, Port}} -> S;
		_ -> get_socket(Rest, Ip, Port)
	end.
