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

-module(rtpproxy_utils).
-author('lemenkov@gmail.com').

-include("../include/common.hrl").

-export([get_fd_pair/1]).

%% Open a pair of UDP ports - N and N+1 (for RTP and RTCP consequently)
get_fd_pair({_, true, SockParams}) ->
	{ok, Ip} = application:get_env(rtpproxy, ipv6),
	get_fd_pair(Ip, SockParams, 10);
get_fd_pair({internal, false, SockParams}) ->
	{ok, Ip} = application:get_env(rtpproxy, internal),
	get_fd_pair(Ip, SockParams ++ [inet6], 10);
get_fd_pair({external, false, SockParams}) ->
	{ok, Ip} = application:get_env(rtpproxy, external),
	get_fd_pair(Ip, SockParams, 10).

%%
%% Private functions
%%

get_fd_pair(Ip, _, 0) ->
	?ERR("Create new socket at ~p FAILED", [Ip]),
	error;
get_fd_pair(Ip, SockParams, NTry) ->
	case gen_udp:open(0, [binary, {ip, Ip}, {active, once}, {raw,1,11,<<1:32/native>>}] ++ SockParams) of
		{ok, Fd} ->
			{ok, {Ip,Port}} = inet:sockname(Fd),
			Port2 = case Port rem 2 of
				0 -> Port + 1;
				1 -> Port - 1
			end,
			case gen_udp:open(Port2, [binary, {ip, Ip}, {active, once}, {raw,1,11,<<1:32/native>>}] ++ SockParams) of
				{ok, Fd2} ->
					if
						Port > Port2 -> {Fd2, Fd};
						Port < Port2 -> {Fd, Fd2}
					end;
				{error, _} ->
					gen_udp:close(Fd),
					get_fd_pair(Ip, SockParams, NTry - 1)
			end;
		{error, _} ->
			get_fd_pair(Ip, SockParams, NTry - 1)
	end.
