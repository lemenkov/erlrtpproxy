
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

% description of media
-record(media, {fd=null, ip=null, port=null}).

start({Parent, From, To}) ->
	gen_server:start(?MODULE, {Parent, From, To}, []).

start_link({Parent, From, To}) ->
	gen_server:start_link(?MODULE, {Parent, From, To}, []).

init ({Parent, {FdFrom, IpFrom, PortFrom}, {FdTo, IpTo, PortTo}}) ->
	print ("::: media[~p] started {~w [~w:~w]} {~w [~w:~w]}~n", [self(), FdFrom, IpFrom, PortFrom, FdTo, IpTo, PortTo]),
	process_flag(trap_exit, true),
	{ok, TRef} = timer:send_interval(10000, self(), ping),
	{ok, {Parent, TRef, #media{fd=FdFrom, ip=IpFrom, port=PortFrom}, #media{fd=FdTo, ip=IpTo, port=PortTo}, rtp}}.

% all other calls
handle_call(_Other, _From, State) ->
	{noreply, State}.

handle_cast(stop, State) ->
	{stop, stop, State};

% all other casts
handle_cast(_Request, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, {Parent, TRef, From, To, _RtpState}) ->
	timer:cancel(TRef),
	gen_udp:close(From#media.fd),
	gen_udp:close(To#media.fd),
	gen_server:cast(Parent, {stop, self()}),
	print("::: media[~w] thread terminated due to reason [~p]~n", [self(), Reason]).

% We received UDP-data on From socket, so we must send in from To-socket (symmetric NAT from the client's POV)
% We must ignore previous state ('rtp' or 'nortp') and set it to 'rtp'
% We use Ip and Port as address for future messages to FdTo
handle_info({udp, Fd, Ip, Port, Msg}, {Parent, TRef, From, To, _RtpState}) when Fd == From#media.fd ->
	gen_udp:send(To#media.fd, From#media.ip, From#media.port, Msg),
	{noreply, {Parent, TRef, From, #media{fd=Fd, ip=Ip, port=Port}, rtp}};

% We received UDP-data on To socket, so we must send in from From-socket (symmetric NAT from the client's POV)
% We must ignore previous state ('rtp' or 'nortp') and set it to 'rtp'
% We use Ip and Port as address for future messages to FdFrom
handle_info({udp, Fd, Ip, Port, Msg}, {Parent, TRef, From, To, _RtpState}) when Fd == To#media.fd ->
	gen_udp:send(From#media.fd, To#media.ip, To#media.port, Msg),
	{noreply, {Parent, TRef, #media{fd=Fd, ip=Ip, port=Port}, To, rtp}};

% setting state to 'nortp'
handle_info(ping, {Parent, TRef, From, To, rtp}) ->
	{noreply, {Parent, TRef, From, To, nortp}};

% We didn't get new messages - we should close this mediastream
handle_info(ping, {Parent, TRef, From, To, nortp}) ->
	print("::: media[~w] timeout~n", [self()]),
	{stop, nortp, {Parent, TRef, From, To, nortp}};

handle_info(Other, State) ->
	print("::: media[~w] Other Info [~p]~n", [self(), Other]),
	{noreply, State}.

print (Format) ->
	print (Format, []).

print (Format, Params) ->
	syslog:send(call, syslog:info(), io_lib:format(Format, Params)).
