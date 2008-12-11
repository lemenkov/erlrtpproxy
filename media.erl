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
-include("config.hrl").
-include("rtcp.hrl").

% description of media:
% * fd - our fd, where we will receive messages from other side
% * ip - real client's ip
% * port - real client's port
-record(media, {fd=null, ip=null, port=null, rtpstate=rtp, lastseen}).
-record(state, {parent, tref, from, fromrtcp, to, tortcp, holdstate=false}).

start(Args) ->
	gen_server:start(?MODULE, Args, []).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

init ({Parent, From, FromRtcp, To, ToRtcp}) ->
	?INFO("started ~p ~p", [From, To]),
%	process_flag(trap_exit, true),
	{ok, TRef} = timer:send_interval(?RTP_TIME_TO_LIVE, ping),
	SafeMakeMedia = fun(Desc) ->
		case Desc of
			{F,I,P} -> #media{fd=F,ip=I,port=P,lastseen=now()};
			_ -> null
		end
	end,
	{ok,
		#state{
			parent=Parent,
			tref=TRef,
			from	=SafeMakeMedia(From),
			fromrtcp=SafeMakeMedia(FromRtcp),
			to	=SafeMakeMedia(To),
			tortcp	=SafeMakeMedia(ToRtcp)
		}
	}.

handle_call(_Other, _From, State) ->
	{noreply, State}.

handle_cast(stop, State) ->
	{stop, stop, State};

handle_cast(hold, State) when State#state.holdstate == false ->
	?INFO("HOLD on", []),
	% We should suppress timer since we shouldn't care this mediastream
	{noreply, State#state{holdstate=true}};

handle_cast(hold, State) when State#state.holdstate == true ->
	?INFO("HOLD off", []),
	% since we suppressed timer earlier, we need to restart it
	{noreply, State#state{holdstate=false}};

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
	lists:map(fun (X) -> case X of null -> ok; _ -> gen_udp:close(X#media.fd) end end, [State#state.from, State#state.fromrtcp, State#state.to, State#state.tortcp]),
%	gen_server:cast(State#state.parent, {stop, self()}),
	?ERR("terminated due to reason [~p]", [Reason]).

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
				(State#state.to)#media{ip=Ip, port=Port, rtpstate=rtp},
				(State#state.to)#media.fd,
				(State#state.from)#media.ip,
				(State#state.from)#media.port};
		Fd == (State#state.to)#media.fd ->
			{	(State#state.from)#media{ip=Ip, port=Port, rtpstate=rtp},
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
	{noreply, State#state{from=F, to=T}};

handle_info({udp, Fd, Ip, Port, Msg}, State) when Fd == (State#state.fromrtcp)#media.fd; Fd == (State#state.tortcp)#media.fd ->
	SafeSendRtcp = fun(F,I,P,M) ->
		if
			I == null; P == null ->
				?WARN("Probably RTCP to ~p from Ip[~p] Port[~p] but we CANNOT send", [Fd, Ip, Port]),
				ok;
			true ->
				?INFO("Probably RTCP to ~p from Ip[~p] Port[~p]", [Fd, Ip, Port]),
				gen_udp:send(F, I, P, Msg)
		end
	end,

	Rtcps = try rtcp:decode(Msg)
	catch
		E:C ->
			{H,M,Ms} = now(),
			file:write_file("./tmp/rtcp_err." ++ atom_to_list(node()) ++ "." ++ integer_to_list(H) ++ "_" ++ integer_to_list(M) ++ "_" ++ integer_to_list(Ms) ++ ".bin", Msg),
			[]
	end,

	if
		Fd == (State#state.fromrtcp)#media.fd ->
			SafeSendRtcp((State#state.to)#media.fd, (State#state.fromrtcp)#media.ip, (State#state.fromrtcp)#media.port, Msg),
%			case lists:keymember(bye, 1, Rtcps) of
%				true -> {stop, stop, State};
%				_ -> {noreply, State#state{tortcp=(State#state.tortcp)#media{ip=Ip,port=Port,rtpstate=rtp,lastseen=now()}}}
%			end;
			{noreply, State#state{tortcp=(State#state.tortcp)#media{ip=Ip,port=Port,rtpstate=rtp,lastseen=now()}}};
		Fd == (State#state.tortcp)#media.fd ->
			SafeSendRtcp((State#state.from)#media.fd, (State#state.tortcp)#media.ip, (State#state.tortcp)#media.port, Msg),
%			case lists:keymember(bye, 1, Rtcps) of
%				true -> {stop, stop, State};
%				_ -> {noreply, State#state{fromrtcp=(State#state.fromrtcp)#media{ip=Ip,port=Port,rtpstate=rtp,lastseen=now()}}}
%			end
			{noreply, State#state{fromrtcp=(State#state.fromrtcp)#media{ip=Ip,port=Port,rtpstate=rtp,lastseen=now()}}}
	end;

handle_info(ping, State) ->
	case {(State#state.from)#media.rtpstate, (State#state.to)#media.rtpstate} of
		{rtp, rtp} ->
			% setting state to 'nortp'
			{noreply, State#state{from=(State#state.from)#media{rtpstate=nortp}, to=(State#state.to)#media{rtpstate=nortp}}};
		_ ->
			% We didn't get new messages since last ping - we should close this mediastream
			% we should rely on rtcp
			case (timer:now_diff(now(),(State#state.fromrtcp)#media.lastseen) > ?RTCP_TIME_TO_LIVE) and (timer:now_diff(now(),(State#state.tortcp)#media.lastseen) > ?RTCP_TIME_TO_LIVE) of
				true ->
					{stop, nortp, State};
				false ->
					{noreply, State}
			end
	end;

handle_info(Other, State) ->
	?WARN("Other Info [~p], State [~p]", [Other, State]),
	{noreply, State}.

