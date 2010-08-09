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

-include("../include/common.hrl").
-include("config.hrl").
%-include("rtcp.hrl").

% description of media:
% * fd - our fd, where we will receive messages from other side
% * ip - real client's ip
% * port - real client's port
-record(media, {fd=null, ip=null, port=null, rtpstate=nortp, lastseen, ssrc=null}).
-record(state, {parent, tref, from, fromrtcp, to, tortcp, fun_send_rtp, fun_send_rtcp, fun_start_acc, holdstate=false, started=null}).

start(Args) ->
	gen_server:start(?MODULE, Args, []).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

init ({Parent, From, FromRtcp, To, ToRtcp}) ->
	?INFO("started ~p ~p", [From, To]),
	process_flag(trap_exit, true),
	{ok, TRef} = timer:send_interval(?RTP_TIME_TO_LIVE*5, ping),

	% Define simple function for safe Media object creation
	SafeMakeMedia = fun
		({F,I,P}) ->
			#media{fd=F,ip=I,port=P,lastseen=now()};
		(_) ->
			null
	end,

	% Define function for sending RTCP and updating state
	SafeSendAndRetState = fun
		(Var1, #media{ip = null, port = null}, Ip, Port, _Msg) ->
			% Probably RTP or RTCP, but we CANNOT send yet.
			Var1#media{ip=Ip, port=Port, rtpstate=rtp, lastseen=now()};
		(Var1, Var2, Ip, Port, Msg) ->
			gen_udp:send(Var1#media.fd, Var2#media.ip, Var2#media.port, Msg),
			Var1#media{ip=Ip, port=Port, rtpstate=rtp, lastseen=now()}
	end,

	% Define function for safe
	FunStartAcc = fun
		(#state{started = null, from = #media{rtpstate = rtp}, to = #media{rtpstate = rtp}} = S) ->
			{noreply, S#state{started=now(), fun_start_acc = fun(S2) -> {noreply, S2} end}};
		(S) ->
			{noreply, S}
	end,

	{ok,
		#state{
			parent=Parent,
			tref=TRef,
			from	= SafeMakeMedia(From),
			fromrtcp= SafeMakeMedia(FromRtcp),
			to	= SafeMakeMedia(To),
			tortcp	= SafeMakeMedia(ToRtcp),
			fun_send_rtp = SafeSendAndRetState,
			fun_send_rtcp= SafeSendAndRetState,
			fun_start_acc= FunStartAcc
		}
	}.

handle_call(?CMD_I, _From, #state{started = Started} = State) ->
	% TODO (acquire information about call state)
%-record(media, {fd=null, ip=null, port=null, rtpstate=rtp, lastseen}).
%-record(state, {parent, tref, from, fromrtcp, to, tortcp, holdstate=false, started}).
	Reply = io_lib:format("CallDuration: ~w", [case Started of null -> "<not started yet>"; _ -> trunc(0.5 + timer:now_diff(erlang:now(), Started) / 1000000) end]),
	{reply, {ok, Reply}, State};

handle_call(_Other, _From, State) ->
	{noreply, State}.

handle_cast(stop, State) ->
	{stop, stop, State};

handle_cast(hold, #state{holdstate = false} = State) ->
	?INFO("HOLD on", []),
	% We should suppress timer since we shouldn't care this mediastream
	{noreply, State#state{fun_send_rtp = fun(Var1, _, Ip, Port, _) -> Var1#media{ip=Ip, port=Port, rtpstate=rtp, lastseen=now()} end, holdstate=true}};

handle_cast(hold, #state{holdstate = true} = State) ->
	?INFO("HOLD off", []),
	% since we suppressed timer earlier, we need to restart it
	% Define function to send RTP/RTCP and update state
	SafeSendAndRetState = fun
		(Var1, #media{ip = null, port = null}, Ip, Port, Msg) ->
			% Probably RTP or RTCP, but we CANNOT send yet.
			Var1#media{ip=Ip, port=Port, rtpstate=rtp, lastseen=now()};
		(Var1, Var2, Ip, Port, Msg) ->
			gen_udp:send(Var1#media.fd, Var2#media.ip, Var2#media.port, Msg),
			Var1#media{ip=Ip, port=Port, rtpstate=rtp, lastseen=now()}
	end,
	{noreply, State#state{fun_send_rtp=SafeSendAndRetState, holdstate=false}};

handle_cast({recording, {start, _Filename}}, State) ->
	% TODO set flag to record this stream RTP
	{noreply, State};

handle_cast({recording, stop}, State) ->
	% TODO stop recording of RTP
	{noreply, State};

% all other casts
handle_cast(_Request, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, #state{parent = Pid, tref = TimerRef, from = From, fromrtcp = FromRtcp, to = To, tortcp = ToRtcp}) ->
	timer:cancel(TimerRef),
	% TODO we should send RTCP BYE here
	lists:map(fun (X) -> case X of null -> ok; _ -> gen_udp:close(X#media.fd) end end, [From, FromRtcp, To, ToRtcp]),
	case Reason of
		nortp -> gen_server:cast(Pid, {stop, timeout, self()});
		_ -> gen_server:cast(Pid, {stop, stop, self()})
	end,
	?ERR("terminated due to reason [~p]", [Reason]).

handle_info({udp, Fd, Ip, Port, Msg}, #state{tortcp = #media{fd = Fd}} = State) ->
	% First, we'll try do decode our RCP packet(s)
	try
		Rtcps = rtcp:decode(Msg),
		case lists:keymember(bye, 1, Rtcps) of
			true ->
				?ERR("We SHOULD terminate this stream due to RTCP BYE", []);
				% Unfortunately, it's not possible due to issues in Asterisk configs
				% which users are unwilling to fix. So we just warn about it.
				% Maybe, in the future, we'll reconsider this behaviour.
%				{stop, rtcp_bye, State};
			_ ->
				ok
		end,

		{noreply, State#state{fromrtcp=(State#state.fun_send_rtcp)(State#state.fromrtcp, State#state.tortcp, Ip, Port, Msg)}}
	catch
		E:C ->
			rtp_utils:dump_packet(node(), self(), Msg),
			?ERR("rtcp:decode(...) error ~p:~p", [E,C]),
			{noreply, State}
	end;


handle_info({udp, Fd, Ip, Port, Msg}, #state{fromrtcp = #media{fd = Fd}} = State) ->
	% First, we'll try do decode our RCP packet(s)
	try
		{ok, Rtcps} = rtcp:decode(Msg),
		case lists:keymember(bye, 1, Rtcps) of
			true ->
				?ERR("We SHOULD terminate this stream due to RTCP BYE", []);
				% Unfortunately, it's not possible due to issues in Asterisk configs
				% which users are unwilling to fix. So we just warn about it.
				% Maybe, in the future, we'll reconsider this behaviour.
%				{stop, rtcp_bye, State};
			_ ->
				ok
		end,

		{noreply, State#state{tortcp=(State#state.fun_send_rtcp)(State#state.tortcp, State#state.fromrtcp, Ip, Port, Msg)}}
	catch
		E:C ->
			rtp_utils:dump_packet(node(), self(), Msg),
			?ERR("rtcp:decode(...) error ~p:~p", [E,C]),
			{noreply, State}
	end;

% We received UDP-data on From or To socket, so we must send in from To or From socket respectively
% (if we not in HOLD state)
% (symmetric NAT from the client's PoV)
% We must ignore previous state ('rtp' or 'nortp') and set it to 'rtp'
% We use Ip and Port as address for future messages to FdTo or FdFrom

% TODO check that message was arrived from valid {Ip, Port}
% TODO check whether message is valid rtp stream
handle_info({udp, Fd, Ip, Port, Msg}, #state{fun_start_acc = FSA, fun_send_rtp = FSR, from = #media{fd = Fd}} = State) ->
	FSA(State#state{to = FSR(State#state.to, State#state.from, Ip, Port, Msg)});
handle_info({udp, Fd, Ip, Port, Msg}, #state{fun_start_acc = FSA, fun_send_rtp = FSR, to = #media{fd = Fd}} = State) ->
	FSA(State#state{from = FSR(State#state.from, State#state.to, Ip, Port, Msg)});

handle_info(ping, #state{from = #media{rtpstate = rtp}, to = #media{rtpstate = rtp}} =  State) ->
	% Both sides are active, so we just set state to 'nortp' and continue
	{noreply, State#state{from=(State#state.from)#media{rtpstate=nortp}, to=(State#state.to)#media{rtpstate=nortp}}};

handle_info(ping, State) ->
	% We didn't get new RTP messages since last ping - we should close this mediastream
	% we should rely on rtcp
%	case (timer:now_diff(now(),(State#state.fromrtcp)#media.lastseen) > ?RTCP_TIME_TO_LIVE) and (timer:now_diff(now(),(State#state.tortcp)#media.lastseen) > ?RTCP_TIME_TO_LIVE) of
%		true ->
%			{stop, nortp, State};
%		false ->
%			{noreply, State}
%	end
	{stop, nortp, State};

handle_info(Other, State) ->
	?WARN("Other Info [~p], State [~p]", [Other, State]),
	{noreply, State}.

