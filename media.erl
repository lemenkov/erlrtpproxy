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

% description of media:
% * fd - our fd, where we will receive messages from other side
% * ip - real client's ip
% * port - real client's port
-record(media, {fd=null, ip=null, port=null}).
-record(state, {parent, tref, from, to, rtpstate=rtp, holdstate=false}).

start({Parent, From, To}) ->
	gen_server:start(?MODULE, {Parent, From, To}, []).

start_link({Parent, From, To}) ->
	gen_server:start_link(?MODULE, {Parent, From, To}, []).

init ({Parent, {FdFrom, IpFrom, PortFrom}, {FdTo, IpTo, PortTo}}) ->
	?PRINT("started {~w [~w:~w]} {~w [~w:~w]}", [FdFrom, IpFrom, PortFrom, FdTo, IpTo, PortTo]),
	process_flag(trap_exit, true),
	{ok, TRef} = timer:send_interval(10000, self(), ping),
	{ok, #state{parent=Parent, tref=TRef, from=#media{fd=FdFrom, ip=IpFrom, port=PortFrom}, to=#media{fd=FdTo, ip=IpTo, port=PortTo}}}.

handle_call(_Other, _From, State) ->
	{noreply, State}.

handle_cast(stop, State) ->
	{stop, stop, State};

handle_cast(hold, State) when State#state.holdstate == false ->
	?PRINT("HOLD on", []),
	% We should suppress timer since we shouldn't care this mediastream
	timer:cancel(State#state.tref),
	{noreply, State#state{tref=null, holdstate=true}};

handle_cast(hold, State) when State#state.holdstate == true ->
	?PRINT("HOLD off", []),
	% since we suppressed timer earlier, we need to restart it
	{ok, TRef} = timer:send_interval(10000, self(), ping),
	{noreply, State#state{tref=TRef, holdstate=false}};

handle_cast({recording, RecState}, State) ->
	case RecState of
		{start, Filename} ->
			% TODO set flag to record this stream RTP
			ok;
		stop ->
			% TODO stop recording of RTP
			ok
	end,
	{noreply, State};

% all other casts
handle_cast(_Request, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, State) ->
	timer:cancel(State#state.tref),
	gen_udp:close((State#state.from)#media.fd),
	gen_udp:close((State#state.to)#media.fd),
	gen_server:cast(State#state.parent, {stop, self()}),
	?PRINT("terminated due to reason [~p]", [Reason]).

% We received UDP-data on From or To socket, so we must send in from To or From socket respectively
% (if we not in HOLD state)
% (symmetric NAT from the client's PoV)
% We must ignore previous state ('rtp' or 'nortp') and set it to 'rtp'
% We use Ip and Port as address for future messages to FdTo or FdFrom
handle_info({udp, Fd, Ip, Port, Msg}, State) when Fd == (State#state.from)#media.fd; Fd == (State#state.to)#media.fd ->
	% TODO check that message was arrived from valid {Ip, Port}
	{F, T, Fd1, Ip1, Port1} = if
		Fd == (State#state.from)#media.fd ->
			{	State#state.from, 
				(State#state.to)#media{ip=Ip, port=Port}, 
				(State#state.to)#media.fd, 
				(State#state.from)#media.ip, 
				(State#state.from)#media.port};
		Fd == (State#state.to)#media.fd ->
			{	(State#state.from)#media{ip=Ip, port=Port}, 
				State#state.to, 
				(State#state.from)#media.fd, 
				(State#state.to)#media.ip, 
				(State#state.to)#media.port}
	end,
	case State#state.holdstate of
		true ->
			% do nothing
			ok;
		false ->
			% TODO check whether message is valid rtp stream
			gen_udp:send(Fd1, Ip1, Port1, Msg)
	end,
	{noreply, State#state{from=F, to=T, rtpstate=rtp}};

handle_info(ping, State) ->
	case State#state.rtpstate of
		rtp ->
			% setting state to 'nortp'
			{noreply, State#state{rtpstate=nortp}};
		nortp ->
			% We didn't get new messages since last ping - we should close this mediastream
			{stop, nortp, State}
	end;

handle_info(Other, State) ->
	?PRINT("Other Info [~p], State [~p]", [Other, State]),
	{noreply, State}.

