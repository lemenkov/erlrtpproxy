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
		to,
		origcmd,
		notify_tag,
		started = true
	}
).

start(Cmd) ->
	% TODO run under supervisor maybe?
	gen_server:start(?MODULE, Cmd, []).

init(#cmd{type = ?CMD_U, callid = CallId, mediaid = MediaId} = Cmd) ->
	process_flag(trap_exit, true),

	% Deferred init
	self() ! {init, Cmd},

	% Register itself only after running deferred init
	gproc:add_global_name({id, CallId, MediaId}),

	{ok, #state{}}.

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
			from = #party{tag = Tag, addr = FAddr, rtcpaddr = FRtcpAddr},
			params = Params} = Cmd,
		#state{
			callid = CallId,
			mediaid = MediaId,
			from = #media{pid = PidF, ip = IpF, port = PortF},
			to = #media{pid = PidT, ip = IpT, port = PortT},
			started = Started,
			notify_tag = NotifyTag,
			tag_f = TagF,
			tag_t = TagT} = State
	) ->
	{Dir, P, Ip, Port} = case Tag of
		TagF -> {from, PidT, IpF, PortF};
		TagT -> {to, PidF, IpT, PortT};
		% Initial set up of a tag_t
		_ when TagT == null -> {to, PidF, IpT, PortT};
		_ -> {notfound, null, null, null}
	end,
	NewStarted = case proplists:get_value(acc, Params) of
		start ->
			gen_server:cast(rtpproxy_notifier, {start, CallId, MediaId, NotifyTag}),
			true;
		_  -> Started
	end,
	case Dir of
		notfound ->
			gen_server:cast(Pid, {reply, Cmd, {error, notfound}}),
			{noreply, State#state{started = NewStarted}};
		_ ->
			RtpParamsAddon = case FAddr of
				null -> [];
				{GuessIp0, GuessPort0} -> [{rtp, {GuessIp0, GuessPort0}}]
			end,

			RtcpParamsAddon = case FRtcpAddr of
				null -> [];
				{GuessIp1, GuessPort1} -> [{rtcp, {GuessIp1, GuessPort1}}]
			end,

			gen_server:cast(P, {update, Params ++ RtpParamsAddon ++ RtcpParamsAddon}),

			% FIXME - this is just wrong. Instead of {Ip, Port + 1} we should supply the other's
			% side IP address.
			gen_server:cast(Pid, {reply, Cmd, {{Ip, Port}, {Ip, Port + 1}}}),

			case Dir of
				to -> {noreply, State#state{tag_t = Tag, started = NewStarted}};
				_ -> {noreply, State#state{started = NewStarted}}
			end
	end;

handle_cast(
		#cmd{
			type = ?CMD_U,
			origin = #origin{pid = Pid},
			callid = CallId,
			mediaid = MediaId,
			params = Params} = Cmd,
		#state{
			callid = CallId,
			mediaid = MediaId,
			notify_tag = NotifyTag,
			started = Started } = State
	) ->
	NewStarted = case proplists:get_value(acc, Params) of
		start ->
			gen_server:cast(rtpproxy_notifier, {start, CallId, MediaId, NotifyTag}),
			true;
		_  -> Started
	end,
	{noreply, State#state{started = NewStarted}};

handle_cast(
		#cmd{
			type = ?CMD_D,
			callid = CallId,
			mediaid = 0,
			from = #party{tag = TagFrom},
			to = To} = Cmd,
		#state{callid = CallId, mediaid = MediaId, tag_f = TagF, tag_t = TagT, tref = TRef, notify_tag = NotifyTag} = State
	) ->

	% FIXME consider checking for direction (is TagFrom  equals to TagF or not?)
	Reason = case To of
		null -> cancel;
		_ -> bye
	end,

	% Send stop message earlier
	gen_server:cast(rtpproxy_notifier, {stop, CallId, MediaId, NotifyTag}),
	gen_server:cast({global, rtpproxy}, {'EXIT', self(), Reason}),

	% Run 30-sec timer for catching the remaining RTP/RTCP (w/o sending
	% them to the other party)
	timer:cancel(TRef),
	{ok, TRef2} = timer:send_interval(30000, Reason),

	% FIXME suppress retransmitting of packets here

	{noreply, State#state{started = false, tref = TRef2}};

handle_cast({start, Pid}, #state{
		callid = CallID,
		mediaid = MediaID,
		from = #media{pid = PidF, rtpstate = SF} = From,
		to = #media{pid = PidT, rtpstate = ST} = To,
		notify_tag = NotifyTag
	} = State)  when Pid == PidF; Pid == PidT ->
	{SF1, ST1} = case Pid of
		PidF -> {rtp, ST};
		PidT -> {SF, rtp}
	end,
	case (SF1 == rtp) and (ST1 == rtp) of
%		true -> gen_server:cast(rtpproxy_notifier, {start, CallID, MediaID, NotifyTag});
		_ -> ok
	end,
	{noreply, State#state{
%			started = (SF1 == rtp) and (ST1 == rtp),
			from = From#media{rtpstate=SF1},
			to = To#media{rtpstate=ST1}
		}
	};

handle_cast({interim_update, Pid}, #state{
		callid = CallID,
		mediaid = MediaID,
		from = #media{pid = PidF, rtpstate = SF} = From,
		to = #media{pid = PidT, rtpstate = ST} = To,
		notify_tag = NotifyTag,
		started = true
	} = State) when Pid == PidF; Pid == PidT ->
	% Both sides are active, so we need to send interim update here
	?INFO("MEDIA: send interim_update from ~p~n", [Pid]),
	gen_server:cast(rtpproxy_notifier, {interim_update, CallID, MediaID, NotifyTag}),
	{SF1, ST1} = case Pid of
		PidF -> {rtp, ST};
		PidT -> {SF, rtp}
	end,
	{noreply, State#state{
			from = From#media{rtpstate=SF1},
			to = To#media{rtpstate=ST1}
		}
	};
handle_cast({interim_update, Pid}, State) ->
	?WARN("MEDIA: discard interim_update from ~p~n", [Pid]),
	{noreply, State};

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

handle_cast({rtcp, Rtcps, PidF}, #state{from = #media{pid=PidF}, to = #media{pid=PidT}} = State) ->
	?INFO("RTCP from ~s: ~s", [State#state.callid, rtp_utils:pp(Rtcps)]),
	gen_server:cast(PidT, {rtcp, Rtcps}),
	{noreply, State};
handle_cast({rtcp, Rtcps, PidT}, #state{from = #media{pid=PidF}, to = #media{pid=PidT}} = State) ->
	?INFO("RTCP from ~s: ~s", [State#state.callid, rtp_utils:pp(Rtcps)]),
	gen_server:cast(PidF, {rtcp, Rtcps}),
	{noreply, State};

handle_cast({started, Pid, {I0, P0}, {I1, P1}}, #state{from = #media{pid = Pid} = From, to = To, origcmd = Cmd} = State) ->
	try_notify_parent(Cmd, From#media{ip = I0, port = P0}, To),
	{noreply, State#state{from = From#media{ip = I0, port = P0}}};

handle_cast({started, Pid, {I0, P0}, {I1, P1}}, #state{from = From, to = #media{pid = Pid} = To, origcmd = Cmd} = State) ->
	try_notify_parent(Cmd, From, To#media{ip = I0, port = P0}),
	{noreply, State#state{to = To#media{ip = I0, port = P0}}};

handle_cast(Other, State) ->
	?ERR("Unmatched cast [~p]", [Other]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, #state{callid = CallId, mediaid = MediaId, tref = TimerRef, from = From, to = To}) ->
	% No need to explicitly unregister from gproc - it does so automatically
	timer:cancel(TimerRef),
	?ERR("terminated due to reason [~p]", [Reason]).

% Ping message
handle_info(ping, #state{from = #media{rtpstate = rtp}, to = #media{rtpstate = rtp}} =  State) ->
	% Both sides are active, so we just set state to 'nortp' and continue
	{noreply, State#state{from=(State#state.from)#media{rtpstate=nortp}, to=(State#state.to)#media{rtpstate=nortp}}};

handle_info(ping, #state{callid = CallId, mediaid = MediaId, notify_tag = NotifyTag} = State) ->
	?ERR("RTP timeout at ~p ~p.", [CallId, MediaId]),
	% We didn't get new RTP messages since last ping - we should close this mediastream
	% we should rely on rtcp
%	case (timer:now_diff(now(),(State#state.fromrtcp)#media.lastseen) > ?RTCP_TIME_TO_LIVE) and (timer:now_diff(now(),(State#state.tortcp)#media.lastseen) > ?RTCP_TIME_TO_LIVE) of
%		true ->
%			{stop, nortp, State};
%		false ->
%			{noreply, State}
%	end
	gen_server:cast(rtpproxy_notifier, {stop, CallId, MediaId, NotifyTag}),
	gen_server:cast({global, rtpproxy}, {'EXIT', self(), nortp}),
	{stop, nortp, State};

handle_info({'EXIT', Pid, Reason}, #state{callid = CallId, mediaid = MediaId, from = #media{pid = Pid}, notify_tag = NotifyTag} = State) ->
	?ERR("RTP From socket died: ~p", [Reason]),
	gen_server:cast(rtpproxy_notifier, {stop, CallId, MediaId, NotifyTag}),
	gen_server:cast({global, rtpproxy}, {'EXIT', self(), Reason}),
	{stop, Reason, State};

handle_info({'EXIT', Pid, Reason}, #state{callid = CallId, mediaid = MediaId, to = #media{pid = Pid}, notify_tag = NotifyTag} = State) ->
	?ERR("RTP To socket died: ~p", [Reason]),
	gen_server:cast(rtpproxy_notifier, {stop, CallId, MediaId, NotifyTag}),
	gen_server:cast({global, rtpproxy}, {'EXIT', self(), Reason}),
	{stop, Reason, State};

handle_info(bye, State) ->
	{stop, bye, State};
handle_info(cancel, State) ->
	{stop, cancel, State};

handle_info({init,
	#cmd{
		type = ?CMD_U,
		callid = CallId,
		mediaid = MediaId,
		from = #party{tag = TagFrom, addr = FAddr, rtcpaddr = FRtcpAddr, proto = TProto},
		params = Params} = Cmd
}, _) ->
	{ok, Ttl} = application:get_env(rtpproxy, ttl),
	{ok, TRef} = timer:send_interval(Ttl, ping),

	{FromDir, ToDir} = proplists:get_value(direction, Params),

	FRtpParamsAddon = case FAddr of
		null -> [];
		{GuessIp0, GuessPort0} -> [{rtp, {GuessIp0, GuessPort0}}]
	end,

	FRtcpParamsAddon = case FRtcpAddr of
		null -> [];
		{GuessIp1, GuessPort1} -> [{rtcp, {GuessIp1, GuessPort1}}]
	end,

	% FIXME use start (w/o linking)
	{ok, PidF} = rtp_socket:start_link([self(), TProto, [{direction, ToDir}]]),
	{ok, PidT} = rtp_socket:start_link([self(), TProto, proplists:delete(direction, Params) ++ [{direction, FromDir}] ++ FRtpParamsAddon ++ FRtcpParamsAddon]),

	{noreply,
		#state{
			callid	= CallId,
			mediaid = MediaId,
			tag_f	= TagFrom,
			tag_t	= null,
			tref	= TRef,
			from	= #media{pid=PidF, rtpstate=nortp},
			to	= #media{pid=PidT, rtpstate=nortp},
			notify_tag = proplists:get_value(notify, Params, []),
			origcmd = Cmd
		}
	};

handle_info(Other, State) ->
	?WARN("Other Info [~p], State [~p]", [Other, State]),
	{noreply, State}.

%%
%% Private functions
%%

try_notify_parent(Cmd, #media{ip = null, port = null}, _) ->
	ok;
try_notify_parent(Cmd, _, #media{ip = null, port = null}) ->
	ok;
try_notify_parent(#cmd{origin = #origin{type = ser, pid = Pid}, callid = CallId, mediaid = MediaId} = Cmd, #media{pid = Pid0, ip = I0, port = P0}, #media{pid = Pid1, ip = I1, port = P1}) ->
	gen_server:cast(Pid0, {neighbour, Pid1}),
	gen_server:cast(Pid1, {neighbour, Pid0}),
	gen_server:cast(Pid, {reply, Cmd, {{I0, P0}, {I1, P1}}}),
	gen_server:cast({global, rtpproxy}, {created, self(), {CallId, MediaId}}),
	ok.
