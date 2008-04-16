
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

-module(media).
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

start({Parent, From, To}) ->
	gen_server:start(?MODULE, {Parent, From, To}, []).

start_link({Parent, From, To}) ->
	gen_server:start_link(?MODULE, {Parent, From, To}, []).

init ({Parent, {FdFrom, IpFrom, PortFrom}, {FdTo, IpTo, PortTo}}) ->
	io:format ("::: media[~p] started {~w [~w:~w]} {~w [~w:~w]}~n", [self(), FdFrom, IpFrom, PortFrom, FdTo, IpTo, PortTo]),
	process_flag(trap_exit, true),
	{ok, TRef} = timer:send_interval(10000, self(), ping),
	{ok, {Parent, TRef, {FdFrom, IpFrom, PortFrom}, {FdTo, IpTo, PortTo}, rtp}}.

% all other calls
handle_call(_Other, _From, State) ->
	{noreply, State}.

handle_cast(stop, {Parent, TRef, {FdFrom, IpFrom, PortFrom}, {FdTo, IpTo, PortTo}, InternalState}) ->
	{stop, stop, {Parent, TRef, {FdFrom, IpFrom, PortFrom}, {FdTo, IpTo, PortTo}, stop}};

% all other casts
handle_cast(_Request, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, {Parent, TRef, {FdFrom, IpFrom, PortFrom}, {FdTo, IpTo, PortTo}, RtpState}) ->
	timer:cancel(TRef),
	gen_udp:close(FdFrom),
	gen_udp:close(FdTo),
	gen_server:cast(Parent, {stop, self()}),
	io:format("::: media[~w] thread terminated due to reason [~p]~n", [self(), Reason]).

handle_info({udp, FdFrom, Ip, Port, Msg}, {Parent, TRef, {FdFrom, IpFrom, PortFrom}, {FdTo, IpTo, PortTo}, RtpState}) ->
%	io:format("::: media[~w] Msg from FdFrom~n", [self()]),
	gen_udp:send(FdTo, IpFrom, PortFrom, Msg),
	{noreply, {Parent, TRef, {FdFrom, IpFrom, PortFrom}, {FdTo, Ip, Port}, rtp}};

handle_info({udp, FdTo, Ip, Port, Msg}, {Parent, TRef, {FdFrom, IpFrom, PortFrom}, {FdTo, IpTo, PortTo}, RtpState}) ->
%	io:format("::: media[~w] Msg from FdTo~n", [self()]),
	gen_udp:send(FdFrom, IpTo, PortTo, Msg),
	{noreply, {Parent, TRef, {FdFrom, Ip, Port}, {FdTo, IpTo, PortTo}, rtp}};

handle_info(ping, {Parent, TRef, {FdFrom, IpFrom, PortFrom}, {FdTo, IpTo, PortTo}, rtp}) ->
	{noreply, {Parent, TRef, {FdFrom, IpFrom, PortFrom}, {FdTo, IpTo, PortTo}, nortp}};

handle_info(ping, {Parent, TRef, {FdFrom, IpFrom, PortFrom}, {FdTo, IpTo, PortTo}, nortp}) ->
	io:format("::: media[~w] ping~n", [self()]),
	{stop, timeout, {Parent, TRef, {FdFrom, IpFrom, PortFrom}, {FdTo, IpTo, PortTo}, timeout}};

handle_info(Other, State) ->
	io:format("::: media[~w] Other Info [~p]~n", [self(), Other]),
	{noreply, State}.
