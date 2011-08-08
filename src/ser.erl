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

-module(ser).
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
-include_lib("erlsyslog/include/erlsyslog.hrl").

start(Args) ->
	gen_server:start(?MODULE, Args, []).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

init (_Unused) ->
	% Load parameters
	{ok, {Ip, Port}} = application:get_env(?MODULE, listen_address),
	{ok, RtpproxyNode} = application:get_env(?MODULE, rtpproxy_node),

	pong = net_adm:ping(RtpproxyNode),

	case gen_udp:open(Port, [{ip, Ip}, {active, true}, list]) of
		{ok, Fd} ->
			?INFO("started at [~s:~w]", [inet_parse:ntoa(Ip), Port]),
			{ok, Fd};
		{error, Reason} ->
			?ERR("interface not started. Reason [~p]", [Reason]),
			{stop, Reason}
	end.

handle_call(_Other, _From, Fd) ->
	{noreply, Fd}.

% Got two addresses (initial Media stream creation)
handle_cast({reply, #cmd{origin = #origin{type = ser, ip = Ip, port = Port}} = Cmd, Answer, _}, Fd) ->
	Data = ser_proto:encode(Cmd, Answer),
	gen_udp:send(Fd, Ip, Port, Data),
	{noreply, Fd};
% TODO deprecate this case
handle_cast({reply, #cmd{origin = #origin{type = ser, ip = Ip, port = Port}} = Cmd, Answer}, Fd) ->
	Data = ser_proto:encode(Cmd, Answer),
	gen_udp:send(Fd, Ip, Port, Data),
	{noreply, Fd};

handle_cast(_Request, Fd) ->
	{noreply, Fd}.

code_change(_OldVsn, Fd, _Extra) ->
	{ok, Fd}.

terminate(Reason, Fd) ->
	gen_udp:close(Fd),
	?ERR("thread terminated due to reason [~p]", [Reason]).

% Fd from which message arrived must be equal to Fd from our state
% Brief introduction of protocol is here: http://rtpproxy.org/wiki/RTPproxyProtocol
handle_info({udp, Fd, Ip, Port, Msg}, Fd) ->
	try ser_proto:parse(Msg, Ip, Port) of
		#cmd{type = ?CMD_V} = Cmd ->
			% Request basic supported rtpproxy protocol version
			% see available versions here:
			% http://sippy.git.sourceforge.net/git/gitweb.cgi?p=sippy/rtpproxy;a=blob;f=rtpp_command.c#l58
			% We provide only basic functionality, currently.
			?INFO("SER cmd: ~p", [Cmd]),
			Data = ser_proto:encode(Cmd, {version, "20040107"}),
			gen_udp:send(Fd, Ip, Port, Data);
		#cmd{type = ?CMD_VF, params=Version} = Cmd ->
			% Request additional rtpproxy protocol extensions
			% TODO we should check version capabilities here
			?INFO("SER cmd: ~p", [Cmd]),
			Data = ser_proto:encode(Cmd, {supported, Version}),
			gen_udp:send(Fd, Ip, Port, Data);
		Cmd ->
			?INFO("SER cmd: ~p", [Cmd]),
			gen_server:cast({global, rtpproxy}, Cmd)
	catch
		throw:{error_syntax, Error} ->
			?ERR("Bad syntax. [~s -> ~s]~n", [Msg, Error]),
			[Cookie|_Rest] = string:tokens(Msg, " ;"),
			Data = ser_proto:encode(Cookie, {error, syntax}),
			gen_udp:send(Fd, Ip, Port, Data);
		E:C ->
			?ERR("Exception. [~s -> ~p:~p]~n", [Msg, E, C]),
			[Cookie|_Rest] = string:tokens(Msg, " ;"),
			Data = ser_proto:encode(Cookie, {error, syntax}),
			gen_udp:send(Fd, Ip, Port, Data)
	end,
	{noreply, Fd};

handle_info(Info, Fd) ->
	?WARN("Info [~w]", [Info]),
	{noreply, Fd}.
