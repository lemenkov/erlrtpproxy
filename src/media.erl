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
-record(state, {parent, callid, tref, from, fromrtcp, to, tortcp, holdstate=false, started=null}).

start(Args) ->
	gen_server:start(?MODULE, Args, []).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

init ({Parent, CallID, From, FromRtcp, To, ToRtcp}) ->
	?INFO("started ~p ~p", [From, To]),
	process_flag(trap_exit, true),
	{ok, TRef} = timer:send_interval(?RTP_TIME_TO_LIVE*5, ping),

	{ok,
		#state{
			parent=Parent,
			callid = CallID,
			tref=TRef,
			from	= safe_make_media(From),
			fromrtcp= safe_make_media(FromRtcp),
			to	= safe_make_media(To),
			tortcp	= safe_make_media(ToRtcp)
		}
	}.

handle_call(?CMD_I, _From, #state{started = Started} = State) ->
	% TODO (acquire information about call state)
%-record(media, {fd=null, ip=null, port=null, rtpstate=rtp, lastseen}).
%-record(state, {parent, tref, from, fromrtcp, to, tortcp, holdstate=false, started}).
	Reply = io_lib:format("CallDuration: ~w", [case Started of null -> "<not started yet>"; _ -> trunc(0.5 + timer:now_diff(erlang:now(), Started) / 1000000) end]),
	{reply, {ok, Reply}, State}.

handle_cast(stop, State) ->
	{stop, stop, State};

handle_cast(hold, #state{holdstate = false} = State) ->
	?INFO("HOLD on", []),
	% We should suppress timer since we shouldn't care this mediastream
	{noreply, State#state{holdstate=true}};

handle_cast(hold, #state{holdstate = true} = State) ->
	?INFO("HOLD off", []),
	% since we suppressed timer earlier, we need to restart it
	{noreply, State#state{holdstate=false}};

handle_cast({recording, {start, _Filename}}, State) ->
	% TODO set flag to record this stream RTP
	{noreply, State};

handle_cast({recording, stop}, State) ->
	% TODO stop recording of RTP
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
		{ok, Rtcps} = rtcp:decode(Msg),
		Msg2 = rtcp_process (Rtcps, State#state.parent),
		{noreply, State#state{fromrtcp=safe_send(State#state.fromrtcp, State#state.tortcp, Ip, Port, Msg2)}}
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
		Msg2 = rtcp_process (Rtcps, State#state.parent),
		?INFO("RTCP from ~s: ~p", [State#state.callid, Msg2]),
		{noreply, State#state{tortcp=safe_send(State#state.tortcp, State#state.fromrtcp, Ip, Port, Msg2)}}
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
handle_info({udp, Fd, Ip, Port, Msg}, #state{from = #media{fd = Fd}} = State) ->
	{noreply, State#state{to = safe_send(State#state.to, State#state.from, Ip, Port, Msg), started=start_acc(State)}};

handle_info({udp, Fd, Ip, Port, Msg}, #state{to = #media{fd = Fd}} = State) ->
	{noreply, State#state{from = safe_send(State#state.from, State#state.to, Ip, Port, Msg), started=start_acc(State)}};

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


%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions %%
%%%%%%%%%%%%%%%%%%%%%%%%

% Define simple function for safe Media object creation
safe_make_media ({F,I,P}) ->
	#media{fd=F,ip=I,port=P,lastseen=now()};
safe_make_media (_) ->
		null.

% Define function for sending RTP/RTCP and updating state
safe_send (Var1, #media{ip = null, port = null}, Ip, Port, _Msg) ->
	% Probably RTP or RTCP, but we CANNOT send yet.
	Var1#media{ip=Ip, port=Port, rtpstate=rtp, lastseen=now()};
safe_send (Var1, Var2, Ip, Port, Msg) ->
	gen_udp:send(Var1#media.fd, Var2#media.ip, Var2#media.port, Msg),
	Var1#media{ip=Ip, port=Port, rtpstate=rtp, lastseen=now()}.

% Define function for safe determinin of starting media
start_acc (#state{started = null, from = #media{rtpstate = rtp}, to = #media{rtpstate = rtp}}) ->
	now();
start_acc (S) ->
	S#state.started.

rtcp_process (Rtcps, Parent) ->
	rtcp_process (Rtcps, [], Parent).
rtcp_process ([], Rtcps, Parent) ->
	lists:map(fun rtcp:decode/1, Rtcps);
rtcp_process ([Rtcp | Rest], Processed, Parent) ->
	NewRtcp = case rtp_utils:get_type(Rtcp) of
		sr -> Rtcp;
		rr -> Rtcp;
		sdes -> Rtcp;
		bye ->
			?ERR("We SHOULD terminate this stream due to RTCP BYE", []),
			% Unfortunately, it's not possible due to issues in Asterisk configs
			% which users are unwilling to fix. So we just warn about it.
			% Maybe, in the future, we'll reconsider this behaviour.
			Rtcp;
		app -> Rtcp;
		xr -> Rtcp;
		_ -> Rtcp
	end,
	rtcp_process (Rest, Processed ++ [NewRtcp],  Parent).
