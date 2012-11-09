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

-record(state, {
		cmd,
		callid,
		mediaid,
		tag,
		rtp,
		type,
		hold = false,
		notify_info
	}
).


start(Cmd) ->
	gen_server:start(?MODULE, [Cmd], []).

init([#cmd{type = ?CMD_U, callid = CallId, mediaid = MediaId, from = #party{tag = Tag}, params = Params} = Cmd]) ->
	% Register itself
	gproc:add_global_name({media, CallId, MediaId, Tag}),
	% Register itself for group call and broadcast commands
	gproc:add_global_property(media, {id, CallId, MediaId}),

	% FIXME
	{ok, I} = application:get_env(rtpproxy, external),
	{ok, Pid} = gen_rtp_channel:start_link(Params ++ [{ip, I}, {rebuildrtp, true}]),

	NotifyInfo = proplists:get_value(notify, Params, []),

	case proplists:get_value(acc, Params, none) of
		none -> ok;
		Acc -> gen_server:cast({global, rtpproxy_notifier}, {Acc, CallId, MediaId, NotifyInfo})
	end,

	{ok, #state{
			cmd = Cmd,
			callid	= CallId,
			mediaid = MediaId,
			tag	= Tag,
			rtp = Pid,
			notify_info = NotifyInfo
		}
	}.

handle_call(Call, _From,  State) ->
	?ERR("Unmatched call [~p]", [Call]),
	{stop,{error,unknown_call},State}.

handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(#cmd{type = ?CMD_D, callid = CallId, mediaid = 0, to = null}, #state{callid = CallId} = State) ->
	{stop, normal, State};
handle_cast(#cmd{type = ?CMD_D, callid = CallId, mediaid = 0}, #state{callid = CallId} = State) ->
	{stop, normal, State};

handle_cast(
	#cmd{type = ?CMD_U, from = #party{addr = {IpAddr,_}}, origin = #origin{pid = Pid}, params = Params} = Cmd,
	#state{callid = CallId, mediaid = MediaId, tag = Tag, notify_info = NotifyInfo} = State
) ->
	case gproc:select([{ { {p,g,phy} , '_' , {id, CallId, MediaId, Tag, '$1', '$2', '$3'} }, [], [['$1','$2','$3']]}]) of
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
		Acc -> gen_server:cast({global, rtpproxy_notifier}, {Acc, CallId, MediaId, NotifyInfo})
	end,
	{noreply, State};

handle_cast(#cmd{type = ?CMD_P, callid = CallId, mediaid = MediaId, to = #party{tag = Tag}, params = Params}, #state{callid = CallId, mediaid = MediaId, tag = Tag, type = PayloadType} = State) ->
	case gproc:select({global,names}, [{ {{n,g,{player, CallId, MediaId, Tag}},'$1','_'}, [], ['$1'] }]) of
		[] ->
			% FIXME empty PayloadInfo for now
			player:start(CallId, MediaId, Tag, ensure_codec(Params, PayloadType));
		[_] -> ok
	end,
	{noreply, State#state{hold = true}};
handle_cast(#cmd{type = ?CMD_S, callid = CallId, mediaid = MediaId, to = #party{tag = Tag}}, #state{callid = CallId, mediaid = MediaId, tag = Tag} = State) ->
	case gproc:select({global,names}, [{ {{n,g,{player, CallId, MediaId, Tag}},'$1','_'}, [], ['$1'] }]) of
		[] -> ok;
		[Pid] -> gen_server:cast(Pid, stop)
	end,
	{noreply, State#state{hold = false}};

handle_cast({Pkt, Ip, Port}, #state{rtp = Pid, hold = false} = State) ->
	gen_server:cast(Pid, {Pkt, Ip, Port}),
	{noreply, State};

handle_cast({_Pkt, _Ip, _Port}, #state{hold = true} = State) ->
	% Music on Hold / Mute
	{noreply, State};

handle_cast({'music-on-hold', Type, Payload}, #state{rtp = Pid} = State) ->
	gen_server:cast(Pid, {raw, Type, Payload}),
	{noreply, State};

handle_cast(Other, State) ->
	?ERR("Unmatched cast [~p]", [Other]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, #state{callid = CallId, mediaid = MediaId, notify_info = NotifyInfo}) ->
	gen_server:cast({global, rtpproxy_notifier}, {stop, CallId, MediaId, NotifyInfo}),
	% No need to explicitly unregister from gproc - it does so automatically
	{memory, Bytes} = erlang:process_info(self(), memory),
	?ERR("terminated due to reason [~p] (allocated ~b bytes)", [Reason, Bytes]).

handle_info({#rtp{payload_type = Type} = Pkt, Ip, Port}, #state{callid = CallId, mediaid = MediaId, tag = Tag} = State) ->
	case gproc:select({global,names}, [{ {{n,g,{media, CallId, MediaId,'$1'}},'$2','_'}, [{'/=', '$1', Tag}], ['$2'] }]) of
		[] -> ok;
		[Pid] -> gen_server:cast(Pid, {Pkt, Ip, Port})
	end,
	{noreply, State#state{type = Type}};
handle_info({Pkt, Ip, Port}, #state{callid = CallId, mediaid = MediaId, tag = Tag} = State) ->
	case gproc:select({global,names}, [{ {{n,g,{media, CallId, MediaId,'$1'}},'$2','_'}, [{'/=', '$1', Tag}], ['$2'] }]) of
		[] -> ok;
		[Pid] -> gen_server:cast(Pid, {Pkt, Ip, Port})
	end,
	{noreply, State};

handle_info({phy, {Ip, PortRtp, PortRtcp}}, #state{callid = CallId, mediaid = MediaId, tag = Tag, cmd = #cmd{origin = #origin{pid = Pid}} = Cmd} = State) ->
	% Store info about physical params
	gproc:add_global_property(phy, {id, CallId, MediaId, Tag, Ip, PortRtp, PortRtcp}),
	% Reply to server
	gen_server:cast(Pid, {reply, Cmd, {{Ip, PortRtp}, {Ip, PortRtcp}}}),
	% No need to store original Cmd any longer
	{noreply, State#state{cmd = null}};

handle_info(interim_update, #state{callid = CallID, mediaid = MediaID, notify_info = NotifyInfo} = State) ->
	gen_server:cast({global, rtpproxy_notifier}, {interim_update, CallID, MediaID, NotifyInfo}),
	{noreply, State}.

%%
%% Internal functions
%%

ensure_codec(Params, CurrPayloadType) ->
	CodecType = rtp_utils:get_codec_from_payload(CurrPayloadType),
	% FIXME we just ignore payload type sent by OpenSIPS/B2BUA and append
	% current one for now
	[{codecs,[CodecType]}|proplists:delete(codecs, Params)].
