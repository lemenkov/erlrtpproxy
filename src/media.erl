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

-record(state, {
		cmd,
		callid,
		mediaid,
		tag,
		rtp,
		type,
		hold = false,
		copy,
		notify_info
	}
).

start(Cmd) ->
	gen_server:start(?MODULE, [Cmd], []).

init([#cmd{type = ?CMD_U, callid = C, mediaid = M, from = #party{tag = T}, params = Params} = Cmd]) ->
	% Register itself
	gproc:add_global_name({media, C, M, T}),
	% Register itself for group call and broadcast commands
	gproc:add_global_property(media, {id, C, M}),

	Ip = case {proplists:get_value(local, Params), proplists:get_value(remote, Params)} of
		{undefined, undefined} ->
			{ok, I} = application:get_env(rtpproxy, external), I;
		{undefined, {_,_,_,_}} ->
			{ok, I} = application:get_env(rtpproxy, external), I;
		{{_,_,_,_}, undefined} ->
%			{ok, I} = application:get_env(rtpproxy, internal), I <- FIXME
			{ok, I} = application:get_env(rtpproxy, external), I
	end,

	{ok, RebuildRtp} = application:get_env(rtpproxy, rebuildrtp),
	{ok, TimeoutEarly} = application:get_env(rtpproxy, ttl_early),
	{ok, Timeout} = application:get_env(rtpproxy, ttl),
	{ok, Pid} = gen_rtp_channel:open(0, Params ++ [{ip, Ip}, {rebuildrtp, RebuildRtp}, {timeout_early, TimeoutEarly*1000}, {timeout, Timeout*1000}]),

	NotifyInfo = proplists:get_value(notify, Params, []),

	Copy = proplists:get_value(copy, Params, false),

	case proplists:get_value(acc, Params, none) of
		none -> ok;
		Acc -> rtpproxy_ctl:acc(Acc, C, M, NotifyInfo)
	end,

	{ok, #state{
			cmd	= Cmd,
			callid	= C,
			mediaid	= M,
			tag	= T,
			rtp	= Pid,
			copy	= Copy,
			notify_info = NotifyInfo
		}
	}.

handle_call(Call, _From,  State) ->
	?ERR("Unmatched call [~p]", [Call]),
	{stop,{error,unknown_call},State}.

handle_cast({keepalive, enable}, #state{rtp = RtpPid} = State) ->
	gen_server:cast(RtpPid, {keepalive, enable}),
	{noreply, State};
handle_cast({keepalive, disable}, #state{rtp = RtpPid} = State) ->
	gen_server:cast(RtpPid, {keepalive, disable}),
	{noreply, State};

handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(#cmd{type = ?CMD_D, callid = C, mediaid = 0, to = null}, #state{callid = C} = State) ->
	{stop, normal, State};
handle_cast(#cmd{type = ?CMD_D, callid = C, mediaid = 0}, #state{callid = C} = State) ->
	{stop, normal, State};

handle_cast(
	#cmd{type = ?CMD_U, from = #party{addr = {IpAddr,_}}, origin = #origin{pid = Pid}, params = Params} = Cmd,
	#state{callid = C, mediaid = M, tag = T, notify_info = NotifyInfo} = State
) ->
	case gproc:select([{ { {p,g,phy} , '_' , {id, C, M, T, '$1', '$2', '$3'} }, [], [['$1','$2','$3']]}]) of
		[[Ip,PortRtp,PortRtcp]] ->
			case IpAddr of
				{0,0,0,0} ->
					gen_server:cast(Pid, {reply, Cmd, {{{0,0,0,0}, PortRtp}, {{0,0,0,0}, PortRtcp}}});
				_ ->
					gen_server:cast(Pid, {reply, Cmd, {{Ip, PortRtp}, {Ip, PortRtcp}}})
			end;
		_ ->
			% FIXME potential race condition on a client
			ok
	end,
	case proplists:get_value(acc, Params, none) of
		none -> ok;
		Acc -> rtpproxy_ctl:acc(Acc, C, M, NotifyInfo)
	end,
	{noreply, State};

handle_cast(#cmd{type = ?CMD_P, callid = C, mediaid = M, to = #party{tag = T}, params = P}, #state{callid = C, mediaid = M, tag = T, type = Type, rtp = RtpPid} = State) ->
	case gproc:select({global,names}, [{{{n, g, {player, C, M, T}}, '$1', '_'}, [], ['$1']}]) of
		[] ->
			CodecType = rtp_utils:get_codec_from_payload(Type),
			% FIXME we just ignore payload type sent by OpenSIPS/B2BUA and append
			% current one for now
			player:start(C, M, T, [{codecs,[CodecType]}|proplists:delete(codecs, P)]),
			gen_server:cast(RtpPid, {keepalive, disable}),
			case gproc:select({global,names}, [{ {{n,g,{media, C, M,'$1'}},'$2','_'}, [{'/=', '$1', T}], ['$2'] }]) of
				[] -> ok;
				[OtherPid] -> gen_server:cast(OtherPid, {keepalive, disable})
			end;
		[_] -> ok
	end,
	{noreply, State#state{hold = true}};
handle_cast(#cmd{type = ?CMD_S, callid = C, mediaid = M, to = #party{tag = T}}, #state{callid = C, mediaid = M, tag = T, rtp = RtpPid} = State) ->
	case gproc:select({global,names}, [{{{n, g, {player, C, M, T}}, '$1', '_'}, [], ['$1']}]) of
		[] -> ok;
		[Pid] ->
			gen_server:cast(Pid, stop),
			gen_server:cast(RtpPid, {keepalive, enable}),
			case gproc:select({global,names}, [{ {{n,g,{media, C, M,'$1'}},'$2','_'}, [{'/=', '$1', T}], ['$2'] }]) of
				[] -> ok;
				[OtherPid] -> gen_server:cast(OtherPid, {keepalive, enable})
			end
	end,
	{noreply, State#state{hold = false}};

handle_cast({'music-on-hold', Pkt}, #state{rtp = Pid, callid = C, mediaid = M, tag = T, copy = Copy} = State) ->
	gen_server:cast(Pid, {Pkt, null, null}),
	Copy andalso gen_server:cast(file_writer, {Pkt, C, M, T}),
	{noreply, State};

handle_cast({Pkt, _Ip, _Port}, #state{rtp = Pid, hold = false, callid = C, mediaid = M, tag = T, copy = Copy} = State) ->
	gen_server:cast(Pid, {Pkt, null, null}),
	Copy andalso gen_server:cast(file_writer, {Pkt, C, M, T}),
	{noreply, State};

handle_cast({_Pkt, _Ip, _Port}, #state{hold = true} = State) ->
	% Music on Hold / Mute
	{noreply, State};

handle_cast(Other, State) ->
	?ERR("Unmatched cast [~p]", [Other]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, #state{rtp = RtpPid, callid = C, mediaid = M, tag = T, notify_info = NotifyInfo, copy = Copy}) ->
	rtpproxy_ctl:acc(stop, C, M, NotifyInfo),
	case gproc:select({global,names}, [{{{n, g, {player, C, M, T}}, '$1', '_'}, [], ['$1']}]) of
		[] -> ok;
		[PlayerPid] -> gen_server:cast(PlayerPid, stop)
	end,
	gen_rtp_channel:close(RtpPid),
	Copy andalso gen_server:cast(file_writer, {eof, C, M, T}),
	% No need to explicitly unregister from gproc - it does so automatically
	{memory, Bytes} = erlang:process_info(self(), memory),
	?ERR("terminated due to reason [~p] (allocated ~b bytes)", [Reason, Bytes]).

handle_info({{Type, _, _} = Pkt, _Ip, _Port}, #state{callid = C, mediaid = M, tag = T} = State) ->
	case gproc:select({global,names}, [{ {{n,g,{media, C, M,'$1'}},'$2','_'}, [{'/=', '$1', T}], ['$2'] }]) of
		[] -> ok;
		[Pid] -> gen_server:cast(Pid, {Pkt, null, null})
	end,
	{noreply, State#state{type = Type}};
handle_info({Pkt, _Ip, _Port}, #state{callid = C, mediaid = M, tag = T} = State) ->
	case gproc:select({global,names}, [{ {{n,g,{media, C, M,'$1'}},'$2','_'}, [{'/=', '$1', T}], ['$2'] }]) of
		[] -> ok;
		[Pid] -> gen_server:cast(Pid, {Pkt, null, null})
	end,
	{noreply, State};

handle_info({phy, {Ip, PortRtp, PortRtcp}}, #state{callid = C, mediaid = M, tag = T, cmd = #cmd{origin = #origin{pid = Pid}} = Cmd} = State) ->
	% Store info about physical params
	gproc:add_global_property(phy, {id, C, M, T, Ip, PortRtp, PortRtcp}),
	% Reply to server
	gen_server:cast(Pid, {reply, Cmd, {{Ip, PortRtp}, {Ip, PortRtcp}}}),
	% No need to store original Cmd any longer
	{noreply, State#state{cmd = null}};

handle_info(interim_update, #state{callid = C, mediaid = M, notify_info = NotifyInfo} = State) ->
	rtpproxy_ctl:acc(interim_update, C, M, NotifyInfo),
	{noreply, State}.
