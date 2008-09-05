
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

-include("common.hrl").

% description of media
-record(media, {fd=null, ip=null, port=null}).

start({Parent, From, To}) ->
	gen_server:start(?MODULE, {Parent, From, To}, []).

start_link({Parent, From, To}) ->
	gen_server:start_link(?MODULE, {Parent, From, To}, []).

init ({Parent, {FdFrom, IpFrom, PortFrom}, {FdTo, IpTo, PortTo}}) ->
	?PRINT("started {~w [~w:~w]} {~w [~w:~w]}", [FdFrom, IpFrom, PortFrom, FdTo, IpTo, PortTo]),
	process_flag(trap_exit, true),
	{ok, TRef} = timer:send_interval(10000, self(), ping),
	{ok, {Parent, TRef, #media{fd=FdFrom, ip=IpFrom, port=PortFrom}, #media{fd=FdTo, ip=IpTo, port=PortTo}, rtp, false}}.

% all other calls
handle_call(_Other, _From, State) ->
	{noreply, State}.

handle_cast(stop, State) ->
	{stop, stop, State};

handle_cast(hold, {Parent, TRef, From, To, _RtpState, false}) ->
	?PRINT("HOLD on", []),
	% We should suppress timer since we shouldn't care this mediastream
	timer:cancel(TRef),
	{noreply, {Parent, null, From, To, _RtpState, true}};

handle_cast(hold, {Parent, null, From, To, _RtpState, true}) ->
	?PRINT("HOLD off", []),
	{ok, TRef} = timer:send_interval(10000, self(), ping),
	{noreply, {Parent, TRef, From, To, _RtpState, false}};

% all other casts
handle_cast(_Request, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, {Parent, TRef, From, To, _RtpState, _HoldState}) ->
	timer:cancel(TRef),
	gen_udp:close(From#media.fd),
	gen_udp:close(To#media.fd),
	gen_server:cast(Parent, {stop, self()}),
	?PRINT("terminated due to reason [~p]", [Reason]).

% We received UDP-data on From or To socket, so we must send in from To or From socket respectively
% (if we not in HOLD state)
% (symmetric NAT from the client's POV)
% We must ignore previous state ('rtp' or 'nortp') and set it to 'rtp'
% We use Ip and Port as address for future messages to FdTo or FdFrom
handle_info({udp, Fd, Ip, Port, Msg}, {Parent, TRef, From, To, _RtpState, HoldState}) ->
%	?PRINT("rtp [~w] [~w] [~w]", [{Fd, Ip, Port}, From, To]),
	{F, T, Fd1, Ip1, Port1} = case (Fd == From#media.fd) of
		true ->
			{From, To#media{ip=Ip, port=Port}, To#media.fd, From#media.ip, From#media.port};
		false ->
			{From#media{ip=Ip, port=Port}, To, From#media.fd, To#media.ip, To#media.port}
	end,
	case HoldState of
		true ->
			% do nothing
			ok;
		false ->
			gen_udp:send(Fd1, Ip1, Port1, Msg)
	end,
	{noreply, {Parent, TRef, F, T, rtp, HoldState}};

handle_info(ping, {Parent, TRef, From, To, RtpState, HoldState}) ->
	case RtpState of
		rtp ->
			% setting state to 'nortp'
			{noreply, {Parent, TRef, From, To, nortp, HoldState}};
		nortp ->
			% We didn't get new messages since last ping - we should close this mediastream
			{stop, nortp, {Parent, TRef, From, To, nortp, HoldState}}
	end;

handle_info(Other, State) ->
	?PRINT("Other Info [~p], State [~p]", [Other, State]),
	{noreply, State}.

