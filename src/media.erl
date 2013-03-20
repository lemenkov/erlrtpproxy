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
-include_lib("rtplib/include/rtp.hrl").
-include_lib("rtplib/include/rtcp.hrl").

-record(state, {
		cmd,
		callid,
		mediaid,
		tag,
		rtp,
		other_rtp = null,
		type = 0, % PCMU by default - required for Music-on-Hold/early media
		tref = null,
		hold = false,
		sibling = null,
		rr = null,
		sr = null,
		local = {null, null, null},
		global = {null, null, null},
		copy,
		notify_info,
		prefill,
		role
	}
).

start(Cmd) ->
	gen_server:start(?MODULE, [Cmd], []).

init([#cmd{type = ?CMD_U, callid = C, mediaid = M, from = #party{tag = T, addr = Addr}, params = Params} = Cmd]) ->
	process_flag(trap_exit, true),

	% Register itself
	gproc:add_global_name({media, C, M, T}),

	% Register itself for group call and broadcast commands
	gproc:add_global_property(media, {C, M, T, null, null, null}),

	Ip = case {proplists:get_value(local, Params), proplists:get_value(remote, Params), proplists:get_value(ipv6, Params)} of
		{_, _, true} ->
			{ok, I} = application:get_env(rtpproxy, ipv6), I;
		{undefined, _, _} ->
			{ok, I} = application:get_env(rtpproxy, external), I;
		{{_,_,_,_}, undefined, _} ->
			{ok, I} = application:get_env(rtpproxy, internal), I
	end,

	{ok, RebuildRtp} = application:get_env(rtpproxy, rebuildrtp),
	{ok, TimeoutEarly} = application:get_env(rtpproxy, ttl_early),
	{ok, Timeout} = application:get_env(rtpproxy, ttl),
	{ok, SendRecvStrategy} = application:get_env(rtpproxy, sendrecv),
	{ok, ActiveStrategy} = application:get_env(rtpproxy, active),
	{ok, RtpPid} = gen_rtp_channel:open(0, Params ++ [{ip, Ip}, {rebuildrtp, RebuildRtp}, {timeout_early, TimeoutEarly*1000}, {timeout, Timeout*1000}, {sendrecv, SendRecvStrategy}, {active, ActiveStrategy}]),

	NotifyInfo = proplists:get_value(notify, Params, []),

	Copy = proplists:get_value(copy, Params, false),

	case proplists:get_value(acc, Params, none) of
		none -> ok;
		Acc -> rtpproxy_ctl:acc(Acc, C, M, NotifyInfo)
	end,

	{Role, Sibling, OtherRtpPid} = case gproc:select({global,names}, [{ {{n,g,{media, C, M,'$1'}},'$2','_'}, [{'/=', '$1', T}], ['$2'] }]) of
		[] ->
			% initial call creation.
			gproc:update_shared_counter({c,g,calls},1),
			{master, null, null};
		[S] ->
			OtherRtpPid0 = gen_server:call(S, {sibling, self(), RtpPid}),
			gen_server:cast(S, {prefill, Addr}),
			{slave, S, OtherRtpPid0}
	end,

	% Set stats timer
	{ok, TRef} = timer:send_interval(1000, get_stats),

	{ok, #state{
			cmd	= Cmd,
			callid	= C,
			mediaid	= M,
			tag	= T,
			tref	= TRef,
			rtp	= RtpPid,
			other_rtp = OtherRtpPid,
			copy	= Copy,
			sibling = Sibling,
			notify_info = NotifyInfo,
			prefill	= Addr,
			role = Role
		}
	}.

% Set sibling
handle_call({sibling, Sibling, OtherRtpPid}, _From, #state{rtp = RtpPid} = State) ->
	% Let's short circuit RTP
	gen_server:call(RtpPid, {rtp_subscriber, OtherRtpPid}),
	gen_server:call(OtherRtpPid, {rtp_subscriber, RtpPid}),
	{reply, RtpPid, State#state{other_rtp = OtherRtpPid, sibling = Sibling}};

handle_call(get_stats, _From, #state{rr = Rr, sr = Sr, rtp = RtpPid} = State) ->
	{RemoteIp, RemoteRtpPort, RemoteRtcpPor, SSRC, Type, RxBytes, RxPackets, TxBytes, TxPackets} = gen_server:call(RtpPid, get_stats),
	{reply, {RemoteIp, RemoteRtpPort, RemoteRtcpPor, SSRC, Type, RxBytes, RxPackets, TxBytes, TxPackets, Rr, Sr}, State#state{type = Type}};

handle_call(Call, _From,  State) ->
	error_logger:error_msg("media ~p: Unmatched call [~p]", [self(), Call]),
	{stop,{error,unknown_call},State}.

handle_cast(
	{Pkt, _Ip, _Port},
	#state{rtp = Pid, hold = false, callid = C, mediaid = M, tag = T, copy = Copy} = State
) ->
	gen_server:cast(Pid, {Pkt, null, null}),
	Copy andalso gen_server:cast(file_writer, {Pkt, C, M, T}),
	{noreply, State};

handle_cast({_Pkt, _Ip, _Port}, #state{hold = true} = State) ->
	% Music on Hold / Mute
	{noreply, State};

handle_cast(
	{'music-on-hold', Pkt},
	#state{rtp = Pid, callid = C, mediaid = M, tag = T, copy = Copy} = State
) ->
	gen_server:cast(Pid, {Pkt, null, null}),
	Copy andalso gen_server:cast(file_writer, {Pkt, C, M, T}),
	{noreply, State};

handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(#cmd{type = ?CMD_D, callid = C, mediaid = 0, to = null}, #state{callid = C} = State) ->
	{stop, normal, State};
handle_cast(#cmd{type = ?CMD_D, callid = C, mediaid = 0}, #state{callid = C} = State) ->
	{stop, normal, State};

handle_cast({prefill, {Ip, Addr}}, #state{rtp = RtpPid, role = slave} = State) ->
	{ok, SendRecvStrategy} = application:get_env(rtpproxy, sendrecv),
	gen_server:cast(RtpPid, {update, [{sendrecv, SendRecvStrategy}, {prefill, {Ip, Addr}}]}),
	{noreply, State};
handle_cast({prefill, {Ip, Addr}}, #state{rtp = RtpPid, role = master, prefill = Addr2, sibling = Sibling} = State) ->
	{ok, SendRecvStrategy} = application:get_env(rtpproxy, sendrecv),
	gen_server:cast(RtpPid, {update, [{sendrecv, SendRecvStrategy}, {prefill, {Ip, Addr}}]}),
	gen_server:cast(Sibling, {prefill, Addr2}),
	{noreply, State};
handle_cast({prefill, _}, State) ->
	{noreply, State};

handle_cast(
	#cmd{type = ?CMD_U, from = #party{addr = Addr}, origin = #origin{pid = Pid}, params = Params} = Cmd,
	#state{rtp = RtpPid, callid = C, mediaid = M, local = Local, notify_info = NotifyInfo} = State
) ->
	case Local of
		{Ip,PortRtp,PortRtcp} ->
			case Addr of
				{{0,0,0,0}, _} ->
					gen_server:cast(Pid, {reply, Cmd, {{{0,0,0,0}, PortRtp}, {{0,0,0,0}, PortRtcp}}});
				_ ->
					gen_server:cast(Pid, {reply, Cmd, {{Ip, PortRtp}, {Ip, PortRtcp}}})
			end;
		_ ->
			% FIXME potential race condition on a client
			ok
	end,
	{ok, SendRecvStrategy} = application:get_env(rtpproxy, sendrecv),
	gen_server:cast(RtpPid, {update, Params ++ [{sendrecv, SendRecvStrategy}]}),
	case proplists:get_value(acc, Params, none) of
		none -> ok;
		Acc -> rtpproxy_ctl:acc(Acc, C, M, NotifyInfo)
	end,
	{noreply, State};

handle_cast(#cmd{type = ?CMD_P, callid = C, mediaid = M, to = #party{tag = T}, params = P}, #state{callid = C, mediaid = M, tag = T, type = Type, rtp = RtpPid, other_rtp = OtherRtpPid, sibling = Sibling} = State) ->
	case gproc:select({global,names}, [{{{n, g, {player, C, M, T}}, '$1', '_'}, [], ['$1']}]) of
		[] ->
			CodecType = rtp_utils:get_codec_from_payload(Type),
			% FIXME we just ignore payload type sent by OpenSIPS/B2BUA and append
			% current one for now
			player:start(C, M, T, [{codecs,[CodecType]}|proplists:delete(codecs, P)]),
			gen_server:cast(RtpPid, {keepalive, disable});
		[_] -> ok
	end,
	gen_server:call(RtpPid, {rtp_subscriber, self()}),
	gen_server:call(OtherRtpPid, {rtp_subscriber, Sibling}),
	{noreply, State#state{hold = true}};
handle_cast(#cmd{type = ?CMD_P, callid = C, mediaid = M, from = #party{tag = T}}, #state{callid = C, mediaid = M, tag = T, rtp = RtpPid} = State) ->
	gen_server:cast(RtpPid, {keepalive, disable}),
	{noreply, State};

handle_cast(#cmd{type = ?CMD_S, callid = C, mediaid = M, to = #party{tag = T}}, #state{callid = C, mediaid = M, tag = T, rtp = RtpPid, other_rtp = OtherRtpPid} = State) ->
	case gproc:select({global,names}, [{{{n, g, {player, C, M, T}}, '$1', '_'}, [], ['$1']}]) of
		[] -> ok;
		[Pid] ->
			gen_server:cast(Pid, stop),
			gen_server:cast(RtpPid, {keepalive, enable})
	end,
	gen_server:call(RtpPid, {rtp_subscriber, OtherRtpPid}),
	gen_server:call(OtherRtpPid, {rtp_subscriber, RtpPid}),
	{noreply, State#state{hold = false}};
handle_cast(#cmd{type = ?CMD_S, callid = C, mediaid = M, from = #party{tag = T}}, #state{callid = C, mediaid = M, tag = T, rtp = RtpPid} = State) ->
	gen_server:cast(RtpPid, {keepalive, enable}),
	{noreply, State};

handle_cast(Other, State) ->
	error_logger:error_msg("media ~p: Unmatched cast [~p]", [self(), Other]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, #state{rtp = RtpPid, callid = C, mediaid = M, tag = T, notify_info = NotifyInfo, copy = Copy, role = Role}) ->
	rtpproxy_ctl:acc(stop, C, M, NotifyInfo),
	case gproc:select({global,names}, [{{{n, g, {player, C, M, T}}, '$1', '_'}, [], ['$1']}]) of
		[] -> ok;
		[PlayerPid] -> gen_server:cast(PlayerPid, stop)
	end,
	gen_rtp_channel:close(RtpPid),
	Copy andalso gen_server:cast(file_writer, {eof, C, M, T}),
	Role == master andalso gproc:update_shared_counter({c,g,calls}, -1),
	% No need to explicitly unregister from gproc - it does so automatically
	{memory, Bytes} = erlang:process_info(self(), memory),
	% FIXME - this is REALLY HORRIBLE but helps to reduce memory consumption in gproc
	{_, GprocMem0} = erlang:process_info(whereis(gproc_dist), memory),
	erlang:garbage_collect(whereis(gproc_dist)),
	{_, GprocMem1} = erlang:process_info(whereis(gproc_dist), memory),
	error_logger:error_msg("media ~p: terminated due to reason [~p] (allocated ~b bytes, Gproc allocated ~b bytes)", [self(), Reason, Bytes, GprocMem0 - GprocMem1]).

handle_info({Pkt, _, _}, #state{sibling = Sibling} = State) when is_binary(Pkt) ->
	gen_server:cast(Sibling, {Pkt, null, null}),
	{noreply, State};
handle_info({#rtp{payload = Payload} = Pkt, _, _}, #state{sibling = Sibling} = State) when is_binary(Payload) ->
	gen_server:cast(Sibling, {Pkt, null, null}),
	{noreply, State};
handle_info({#rtp{payload = #dtmf{} = Payload} = Pkt, _, _}, #state{callid = C, mediaid = M, tag = T, sibling = Sibling} = State) ->
	gen_server:cast(Sibling, {Pkt, null, null}),
	error_logger:warning_msg("DTMF: ~p from C[~p] T:M[~p:~p]~n", [Payload, C, T, M]),
	{noreply, State};
handle_info({#rtcp{payloads = Rtcps} = Pkt, _, _}, #state{callid = C, mediaid = M, tag = T, sibling = Sibling} = State) ->
	gen_server:cast(Sibling, {Pkt, null, null}),
	% FIXME accumulate info from the RTCP
	Sr = rtp_utils:take(Rtcps, sr),
	Rr = rtp_utils:take(Rtcps, rr),
	error_logger:warning_msg("RTCP: ~p from C[~p] T:M[~p:~p]~n", [rtp_utils:pp(Pkt), C, T, M]),
	{noreply, State#state{rr = Rr, sr = Sr}};

handle_info({phy, {Ip, PortRtp, PortRtcp}}, #state{callid = C, mediaid = M, tag = T, cmd = #cmd{origin = #origin{pid = Pid}} = Cmd} = State) ->
	% Store info about physical params
	[[Payload, Remote]] = gproc:select([{{{p,g,media},'_',{C,M,T,'$1','_','$2'}}, [], [['$1','$2']]}]),
	gproc:unreg({p,g,media}),
	gproc:add_global_property(media, {C, M, T, Payload, {Ip, PortRtp, PortRtcp}, Remote}),
	% Reply to server
	gen_server:cast(Pid, {reply, Cmd, {{Ip, PortRtp}, {Ip, PortRtcp}}}),
	% No need to store original Cmd any longer
	{noreply, State#state{cmd = null, local = {Ip, PortRtp, PortRtcp}}};

handle_info(interim_update, #state{callid = C, mediaid = M, notify_info = NotifyInfo} = State) ->
	rtpproxy_ctl:acc(interim_update, C, M, NotifyInfo),
	{noreply, State};

handle_info(get_stats, #state{callid = C, mediaid = M, tag = T, rtp = RtpPid, other_rtp = OtherRtpPid, local = Local, global = Global} = State) ->
	{Ip, RtpPort, RtcpPort, _SSRC, Type, _, _, _, _} = gen_server:call(RtpPid, get_stats),
	(Global == {Ip, RtpPort, RtcpPort}) andalso
	begin
		% FIXME what if I actually want to receive all RTPs?
		% This also ruins ZRTP/SRTP
		(OtherRtpPid /= null) andalso gen_server:call(OtherRtpPid, {rtp_subscriber, gen_server:call(RtpPid, get_rtp_phy)}),
		gproc:unreg({p,g,media}),
		gproc:add_global_property(media, {C, M, T, Type, Local, {Ip, RtpPort, RtcpPort}})
	end,
	{noreply, State#state{type = Type, global = {Ip, RtpPort, RtcpPort}}};

handle_info({'EXIT', Pid, _}, #state{rtp = Pid, sibling = Sibling, tref = TRef} = State) ->
	gen_server:cast(Sibling, stop),
	timer:cancel(TRef),
	{stop, normal, State}.
