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
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include("../include/common.hrl").

% Milliseconds - 105 seconds
-define(RTP_TIME_TO_LIVE, 105000).

%-include("rtcp.hrl").

% description of media:
% * fd - our fd, where we will receive messages from other side
% * ip - real client's ip
% * port - real client's port
-record(media, {pid=null, ip=null, port=null, rtpstate=nortp}).
-record(state, {
		callid,
		mediaid,
		tag_f,
		tag_t,
		tref,
		from,
		fromrtcp,
		to,
		tortcp,
		started = false
	}
).

start(Cmd) ->
	% TODO run under supervisor maybe?
	gen_server:start(?MODULE, Cmd, []).

init (
	#cmd{
		type = ?CMD_U,
		origin = #origin{pid = Pid},
		callid = CallId,
		mediaid = MediaId,
		from = #party{tag = TagFrom, addr = {GuessIp, GuessPort}},
		params = Params} = Cmd
	) ->
	% TODO just choose the first IP address for now
	[MainIp | _Rest ]  = rtpproxy_utils:get_ipaddrs(),
	{ok, TRef} = timer:send_interval(?RTP_TIME_TO_LIVE, ping),

	{Fd0, Fd1, Fd2, Fd3} = rtpproxy_utils:get_fd_quadruple(MainIp),

	[P0, P1, P2, P3]  = lists:map(fun(X) -> {ok, {_I, P}} = inet:sockname(X), P end, [Fd0, Fd1, Fd2, Fd3]),
	?INFO("~s started at ~s, with  F {~p,~p} T {~p,~p}", [CallId, inet_parse:ntoa(MainIp), P0, P1, P2, P3]),

	{ok, {I0, P0}} = inet:sockname(Fd0),
	{ok, {I1, P1}} = inet:sockname(Fd1),
	{ok, {I2, P2}} = inet:sockname(Fd2),
	{ok, {I3, P3}} = inet:sockname(Fd3),
	gen_server:cast(Pid, {reply, Cmd, {I0, P0}, {I2, P2}}),

	% FIXME use start (w/o linking)
	{ok, Pid0} = gen_rtp_socket:start_link([self(), Fd0, udp, rtp, Params]),
	{ok, Pid1} = gen_rtp_socket:start_link([self(), Fd1, udp, rtcp, []]),
	{ok, Pid2} = gen_rtp_socket:start_link([self(), Fd2, udp, rtp, Params ++ [{ip, GuessIp}, {port, GuessPort}]]),
	{ok, Pid3} = gen_rtp_socket:start_link([self(), Fd3, udp, rtcp, []]),

	gen_udp:controlling_process(Fd0, Pid0),
	gen_udp:controlling_process(Fd1, Pid1),
	gen_udp:controlling_process(Fd2, Pid2),
	gen_udp:controlling_process(Fd3, Pid3),

	gen_server:call(Pid0, {neighbour, Pid2}),
	gen_server:call(Pid2, {neighbour, Pid0}),

	gen_server:call(Pid1, {neighbour, Pid3}),
	gen_server:call(Pid3, {neighbour, Pid1}),

	% Register at the rtpproxy
	gen_server:cast({global, rtpproxy}, {created, self(), {CallId, MediaId}}),

	{ok,
		#state{
			callid	= CallId,
			mediaid = MediaId,
			tag_f	= TagFrom,
			tag_t	= null,
			tref	= TRef,
			from	= #media{pid=Pid0, ip=I0, port=P0, rtpstate=nortp},
			fromrtcp= #media{pid=Pid1, ip=I1, port=P1, rtpstate=nortp},
			to	= #media{pid=Pid2, ip=I2, port=P2, rtpstate=nortp},
			tortcp	= #media{pid=Pid3, ip=I3, port=P3, rtpstate=nortp}
		}
	}.

handle_call(?CMD_Q, _From, #state{started = Started} = State) ->
	% TODO (acquire information about call state)
	% sprintf(buf, "%s %d %lu %lu %lu %lu\n", cookie, spa->ttl, spa->pcount[idx], spa->pcount[NOT(idx)], spa->pcount[2], spa->pcount[3]);
	Reply = io_lib:format("CallDuration: ~w", [case Started of false -> "<not started yet>"; _ -> trunc(0.5 + timer:now_diff(erlang:now(), Started) / 1000000) end]),
	{reply, {ok, Reply}, State}.

% FIXME move some logic into frontend - leave here only common part
handle_cast(
		#cmd{
			type = ?CMD_U,
			origin = #origin{pid = Pid},
			callid = CallId,
			mediaid = MediaId,
			from = #party{tag = Tag, addr = {GuessIp, GuessPort}},
			to = CmdTo,
			params = Params} = Cmd,
		#state{
			callid = CallId,
			mediaid = MediaId,
			from = #media{pid = PidF, ip = IpF, port = PortF} = From,
			to = #media{pid = PidT, ip = IpT, port = PortT} = To,
			tag_f = TagF,
			tag_t = TagT} = State
	) ->
	{Dir, P, Ip, Port} = case Tag of
		TagF -> {from, PidT, IpF, PortF};
		TagT -> {to, PidF, IpT, PortT};
		% Initial set up of a tag_t
		_ when TagT == null -> {to, PidF, IpT, PortT};
		_ -> {notfound, notfound, notfound, notfound}
	end,
	case Dir of
		notfound ->
			gen_server:cast(Pid, {reply, Cmd, {error, notfound}}),
			{noreply, State};
		_ ->
			gen_server:cast(P, {update, Params ++ [{ip, GuessIp}, {port, GuessPort}]}),

			gen_server:cast(Pid, {reply, Cmd, {Ip, Port}}),

			case Dir of
				to -> {noreply, State#state{tag_t = Tag}};
				_ -> {noreply, State}
			end
	end;

handle_cast(
		#cmd{
			type = ?CMD_D,
			origin = #origin{pid = Pid},
			callid = CallId,
			mediaid = 0,
			from = #party{tag = TagFrom},
			to = To} = Cmd,
		#state{callid = CallId, tag_f = TagF, tag_t = TagT} = State
	) ->
	% FIXME consider checking for direction (is TagFrom  equals to TagF or not?)
	case To of
		null -> {stop, cancel, State};
		_ -> {stop, bye, State}
	end;

handle_cast({start, Pid}, #state{
		callid = CallID,
		mediaid = MediaID,
		from = #media{pid = PidF, rtpstate = SF} = From,
		to = #media{pid = PidT, rtpstate = ST} = To
	} = State)  when Pid == PidF; Pid == PidT ->
	{SF1, ST1} = case Pid of
		PidF -> {rtp, ST};
		PidT -> {SF, rtp}
	end,
	case (SF1 == rtp) and (ST1 == rtp) of
		true -> gen_server:cast(rtpproxy_radius, {start, CallID, MediaID});
		_ -> ok
	end,
	{noreply, State#state{
			started = (SF1 == rtp) and (ST1 == rtp),
			from = From#media{rtpstate=SF1},
			to = To#media{rtpstate=ST1}
		}
	};

handle_cast({interim_update, Pid}, #state{
		callid = CallID,
		mediaid = MediaID,
		from = #media{pid = PidF, rtpstate = SF} = From,
		to = #media{pid = PidT, rtpstate = ST} = To,
		started = true
	} = State) when Pid == PidF; Pid == PidT ->
	% Both sides are active, so we need to send interim update here
	gen_server:cast(rtpproxy_radius, {interim_update, CallID, MediaID}),
	{SF1, ST1} = case Pid of
		PidF -> {rtp, ST};
		PidT -> {SF, rtp}
	end,
	{noreply, State#state{
			from = From#media{rtpstate=SF1},
			to = To#media{rtpstate=ST1}
		}
	};

handle_cast({stop, Pid, Reason}, #state{
		from = #media{pid = PidF, rtpstate = SF} = From,
		to = #media{pid = PidT, rtpstate = ST} = To
	} = State) when Pid == PidF; Pid == PidT ->
	{SF1, ST1} = case Pid of
		PidF -> {nortp, ST};
		PidT -> {SF, nortp}
	end,
	case (SF1 == nortp) and (ST1 == nortp) of
		true -> {stop, stop, State};
		_ -> {noreply, State#state{
					from = From#media{rtpstate=SF1},
					to = To#media{rtpstate=ST1}
				}
			}
	end;

handle_cast({rtcp, Rtcps, From, To}, State) ->
	?INFO("RTCP from ~s: ~s", [State#state.callid, lists:map (fun rtp_utils:pp/1, Rtcps)]),
	gen_server:cast(To, {rtcp, Rtcps}),
	{noreply, State};

handle_cast(_Other, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, #state{callid = CallId, mediaid = MediaId, tref = TimerRef, from = From, fromrtcp = FromRtcp, to = To, tortcp = ToRtcp}) ->
	timer:cancel(TimerRef),
	% TODO we should send RTCP BYE here
	lists:map(fun (X) -> gen_server:cast(X#media.pid, stop) end, [From, FromRtcp, To, ToRtcp]),

	gen_server:cast({global, rtpproxy}, {'EXIT', self(), Reason}),
	gen_server:cast(rtpproxy_radius, {stop, CallId, MediaId}),

	?ERR("terminated due to reason [~p]", [Reason]).

% Ping message
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

