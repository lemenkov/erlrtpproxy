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
	{ok, Fd} = gen_udp:open(Port, [{ip, IPv6}, {active, true}, binary, inet6]),
	error_logger:info_msg("UDP listener started at [~s:~w]~n", [inet_parse:ntoa(IPv6), Port]),
	{ok, Fd};
init ([{I0, I1, I2, I3} = IPv4, Port]) when
	is_integer(I0), I0 >= 0, I0 < 256,
	is_integer(I1), I1 >= 0, I1 < 256,
	is_integer(I2), I2 >= 0, I2 < 256,
	is_integer(I3), I3 >= 0, I3 < 256 ->
	process_flag(trap_exit, true),
	{ok, Fd} = gen_udp:open(Port, [{ip, IPv4}, {active, true}, binary]),
	error_logger:info_msg("UDP listener started at [~s:~w]~n", [inet_parse:ntoa(IPv4), Port]),
	{ok, Fd}.

handle_call(Other, _From, State) ->
	error_logger:warning_msg("UDP listener: strange call: ~p~n", [Other]),
	{noreply, State}.

handle_cast(#response{origin = #origin{type = ser, ip = Ip, port = Port}} = Response, Fd) ->
	Data = ser_proto:encode(Response),
	gen_udp:send(Fd, Ip, Port, Data),
	{noreply, Fd};

handle_cast({reply, Cmd = #cmd{origin = #origin{type = ser, ip = Ip, port = Port}}, {Addr1, Addr2}}, Fd) ->
	error_logger:info_msg("SER reply ~p~n", [{Addr1, Addr2}]),
	Data = ser_proto:encode(#response{cookie = Cmd#cmd.cookie, origin = Cmd#cmd.origin, type = reply, data = {Addr1, Addr2}}),
	gen_udp:send(Fd, Ip, Port, Data),
	{noreply, Fd};

handle_cast({reply, Cmd = #cmd{origin = #origin{type = ser, ip = Ip, port = Port}}, ok}, Fd) ->
	error_logger:info_msg("SER reply ok~n"),
	Data = ser_proto:encode(#response{cookie = Cmd#cmd.cookie, origin = Cmd#cmd.origin, type = reply, data = ok}),
	gen_udp:send(Fd, Ip, Port, Data),
	{noreply, Fd};

handle_cast(stop, State) ->
	{stop, stop, State};

handle_cast(Other, State) ->
	error_logger:warning_msg("UDP listener: strange cast: ~p~n", [Other]),
	{noreply, State}.

% Fd from which message arrived must be equal to Fd from our state
handle_info({udp, Fd, Ip, Port, Msg}, Fd) ->
	try ser_proto:decode(Msg) of
		#cmd{cookie = Cookie, origin = Origin, type = ?CMD_V} ->
			% Request basic supported rtpproxy protocol version
			% see available versions here:
			% http://sippy.git.sourceforge.net/git/gitweb.cgi?p=sippy/rtpproxy;a=blob;f=rtpp_command.c#l58
			% We provide only basic functionality, currently.
			error_logger:info_msg("SER cmd V~n"),
			Data = ser_proto:encode(#response{cookie = Cookie, origin = Origin, type = reply, data = {version, <<"20040107">>}}),
			gen_udp:send(Fd, Ip, Port, Data);
		#cmd{cookie = Cookie, origin = Origin, type = ?CMD_VF, params=Version} ->
			% Request additional rtpproxy protocol extensions
			error_logger:info_msg("SER cmd VF: ~s~n", [Version]),
			Data = ser_proto:encode(#response{cookie = Cookie, origin = Origin, type = reply, data = supported}),
			gen_udp:send(Fd, Ip, Port, Data);
		#cmd{origin = Origin, type = ?CMD_L} = Cmd ->
			error_logger:info_msg("SER cmd: ~p~n", [Cmd]),
			gen_server:cast({global, rtpproxy}, Cmd#cmd{origin = Origin#origin{ip=Ip, port=Port, pid = self()}, type = ?CMD_U});
		#cmd{origin = Origin} = Cmd ->
			error_logger:info_msg("SER cmd: ~p~n", [Cmd]),
			gen_server:cast({global, rtpproxy}, Cmd#cmd{origin = Origin#origin{ip=Ip, port=Port, pid = self()}})
	catch
		throw:{error_syntax, Error} ->
			error_logger:error_msg("Bad syntax. [~s -> ~s]~n", [Msg, Error]),
			Data = ser_proto:encode({error, syntax, Msg}),
			gen_udp:send(Fd, Ip, Port, Data);
		E:C ->
			error_logger:error_msg("Exception. [~s -> ~p:~p]~n", [Msg, E, C]),
			Data = ser_proto:encode({error, syntax, Msg}),
			gen_udp:send(Fd, Ip, Port, Data)
	end,
	{noreply, Fd};

handle_info(Info, State) ->
	error_logger:warning_msg("UDP listener: strange info: ~p~n", [Info]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, Fd) ->
	gen_udp:close(Fd),
	error_logger:error_msg("UDP listener closed: ~p~n", [Reason]),
	ok.

