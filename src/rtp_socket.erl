%%%----------------------------------------------------------------------
%%% Copyright (c) 2011 Peter Lemenkov <lemenkov@gmail.com>
%%%
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification,
%%% are permitted provided that the following conditions are met:
%%%
%%% * Redistributions of source code must retain the above copyright notice, this
%%% list of conditions and the following disclaimer.
%%% * Redistributions in binary form must reproduce the above copyright notice,
%%% this list of conditions and the following disclaimer in the documentation
%%% and/or other materials provided with the distribution.
%%% * Neither the names of its contributors may be used to endorse or promote
%%% products derived from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ''AS IS'' AND ANY
%%% EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
%%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%
%%%----------------------------------------------------------------------

-module(rtp_socket).
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
-include_lib("rtplib/include/rtp.hrl").
-include_lib("rtplib/include/rtcp.hrl").

% Milliseconds
-define(RTP_TIME_TO_LIVE, 150000).
% Milliseconds
-define(INTERIM_UPDATE, 30000).

-record(state, {
		parent,
		fd,
		ssrc,
		rtcp,
		transport,
		proto,
		ipf,
		portf,
		ipt,
		portt,
		ipf1,
		portf1,
		ipt1,
		portt1,
		started,
		started1,
		lastseen,
		alive,
		weak,
		symmetric,
		transcode,
		codecs = null,
		tref,
		neighbour = null
	}
).

start(Args) ->
	gen_server:start(?MODULE, Args, []).
start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

init ([Parent, Transport, Params]) ->
	process_flag(trap_exit, true),
	% FIXME allow explicitly set address (IP and Port) for both RTP and RTCP sockets
	SockParams = proplists:get_value(sockparams, Params, []),
	% FIXME open socket according Transport - don't hardcode UDP
	Direction = proplists:get_value(direction, Params),
	IsIpv6 = proplists:get_value(ipv6, Params, false),
	{Fd0, Fd1} = utils:get_fd_pair({Direction, IsIpv6, SockParams}),

	{ok, TRef} = timer:send_interval(?INTERIM_UPDATE, interim_update),

	Weak = proplists:get_value(weak, Params, false),
	Symmetric = proplists:get_value(symmetric, Params, true),
	TryTranscode = proplists:get_value(transcode, Params, null),

	{Codecs, Transcode} = case TryTranscode of
		null -> {[], null};
		_ ->
			C1 = lists:map(
					fun(CodecDesc) ->
						case codec:start_link(CodecDesc) of
							{ok, Codec} ->{rtp_utils:get_payload_from_codec(CodecDesc), Codec};
							_ -> {rtp_utils:get_payload_from_codec(CodecDesc), passthrough}
						end
					end,
					proplists:get_value(codecs, Params, [])
			),
			T1 = case TryTranscode of
				{'PCMU',8000,1} -> {?RTP_PAYLOAD_PCMU, proplists:get_value(?RTP_PAYLOAD_PCMU,C1)};
				{'GSM',8000,1} -> {?RTP_PAYLOAD_GSM, proplists:get_value(?RTP_PAYLOAD_GSM,C1)};
				{'PCMA',8000,1} -> {?RTP_PAYLOAD_PCMA, proplists:get_value(?RTP_PAYLOAD_PCMA,C1)};
				{'G722',8000,1} -> {?RTP_PAYLOAD_G722, proplists:get_value(?RTP_PAYLOAD_G722,C1)};
				{'DVI4',8000,1} -> {?RTP_PAYLOAD_DVI4_8KHz, proplists:get_value(?RTP_PAYLOAD_DVI4_8KHz,C1)};
				{'DVI4',11025,1} -> {?RTP_PAYLOAD_DVI4_11KHz, proplists:get_value(?RTP_PAYLOAD_DVI4_11KHz,C1)};
				{'DVI4',16000,1} -> {?RTP_PAYLOAD_DVI4_16KHz, proplists:get_value(?RTP_PAYLOAD_DVI4_16KHz,C1)};
				{'DVI4',22050,1} -> {?RTP_PAYLOAD_DVI4_22KHz, proplists:get_value(?RTP_PAYLOAD_DVI4_22KHz,C1)};
				_ -> undefined
			end,
			case T1 of
				undefined ->
					lists:foreach(fun
							({_, passthrough}) -> ok;
							({_, Codec}) -> codec:close(Codec);
							(_) -> ok
						end, C1),
					{[], null};
				_ ->
					{C1, T1}
			end
	end,

	% Get probable IP and port for RTP
	{Ip, Port} = proplists:get_value(rtp, Params, {null, null}),
	% Get probable IP and port for RTCP
	{Ip1, Port1} = proplists:get_value(rtcp, Params, {null, null}),

	{ok, {I0, P0}} = inet:sockname(Fd0),
	{ok, {I1, P1}} = inet:sockname(Fd1),

	gen_server:cast(Parent, {started, self(), {I0, P0}, {I1, P1}}),

	{ok, #state{
			parent = Parent,
			fd = Fd0,
			rtcp = Fd1,
			transport = Transport,
			ipf = null,
			portf = null,
			ipt = Ip,
			portt = Port,
			ipf1 = null,
			portf1 = null,
			ipt1 = Ip1,
			portt1 = Port1,
			started = false,
			started1 = false,
			lastseen = null,
			alive = false,
			weak = Weak,
			symmetric = Symmetric,
			transcode = Transcode,
			codecs = Codecs,
			tref = TRef
		}
	}.

handle_call(_Call, _From, State) ->
	{stop, bad_call, State}.

handle_cast({rtp, _Pkts}, #state{ipt = null, portt = null} = State) ->
	{noreply, State};
handle_cast({rtp, Rtp}, #state{fd = Fd, ipt = Ip, portt = Port, transcode = null} = State) ->
	Msg = rtp:encode(Rtp),
	% FIXME use Transport
	gen_udp:send(Fd, Ip, Port, Msg),
	{noreply, State};
handle_cast({rtp, Rtp}, #state{fd = Fd, ipt = Ip, portt = Port, transcode = Transcode, codecs = Codecs} = State) ->
	{rtp, Rtp2} = transcode(Rtp, Transcode, Codecs),
	Msg = rtp:encode(Rtp2),
	% FIXME use Transport
	gen_udp:send(Fd, Ip, Port, Msg),
	{noreply, State};

handle_cast({rtcp, Pkts}, #state{ipt1 = null, portt1 = null} = State) ->
	{noreply, State};
handle_cast({rtcp, Rtcp}, #state{rtcp = Fd, ipt1 = Ip, portt1 = Port} = State) ->
	% FIXME Do something with RTCP before sending it further
	Msg = rtcp:encode(Rtcp),
	% FIXME use Transport
	gen_udp:send(Fd, Ip, Port, Msg),
	{noreply, State};

handle_cast({update, Params}, State) ->
	Weak = proplists:get_value(weak, Params, false),
	Symmetric = proplists:get_value(symmetric, Params, true),
	Transcode = proplists:get_value(transcode, Params, null),

	% Get probable IP and port for RTP
	{Ip, Port} = proplists:get_value(rtp, Params, {null, null}),
	% Get probable IP and port for RTCP
	{Ip1, Port1} = proplists:get_value(rtcp, Params, {null, null}),

	case State#state.started of
		true ->
			{noreply, State#state{
					weak = Weak,
					symmetric = Symmetric
				}
			};
		_ ->
			{noreply, State#state{
					ipt = Ip,
					portt = Port,
					ipt1 = Ip1,
					portt1 = Port1,
					weak = Weak,
					symmetric = Symmetric
				}
			}
	end;

handle_cast({neighbour, Pid}, State) when is_pid(Pid) ->
	{noreply, State#state{neighbour = Pid}}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, #state{fd = Fd0, rtcp = Fd1, transport = Transport, tref = TRef, codecs = Codecs}) ->
	timer:cancel(TRef),
	Type = case Transport of
		udp -> gen_udp;
		tcp -> gen_tcp;
		sctp -> gen_sctp
	end,
	Type:close(Fd0),
	% We must send RTCP bye here
	Type:close(Fd1),
	lists:foreach(fun
			({_, passthrough}) -> ok;
			({_, Codec}) -> codec:close(Codec)
		end, Codecs),
	ok.

handle_info({udp, Fd, Ip, Port, Msg}, #state{fd = Fd, ssrc = SSRC, started = true, weak = true, neighbour = Neighbour} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		{ok, Rtp} = rtp:decode(Msg),
		gen_server:cast(Neighbour, {rtp, Rtp}),
		{noreply, State#state{ipt = Ip, portt = Port, lastseen = now(), alive = true}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;

handle_info({udp, Fd, Ip, Port, Msg}, #state{fd = Fd, ssrc = SSRC, started = true, weak = false, ipf = Ip, portf = Port, neighbour = Neighbour} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		{ok, Rtp} = rtp:decode(Msg),
		gen_server:cast(Neighbour, {rtp, Rtp}),
		{noreply, State#state{lastseen = now(), alive = true}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;
handle_info({udp, Fd, Ip, Port, Msg}, #state{fd = Fd, ssrc = SSRC, started = true, weak = false, neighbour = Neighbour} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		{ok, Rtp} = rtp:decode(Msg),
		case ensure_ssrc(SSRC, Rtp) of
			true ->
				?WARN("RTP addr changed, but known SSRC found. Updating addr.", []),
				gen_server:cast(Neighbour, {rtp, Rtp}),
				{noreply, State#state{ipf = Ip, portf = Port, ipt = Ip, portt = Port, lastseen = now(), alive = true}};
			_ ->
				?ERR("Disallow data from strange source with different SSRC", []),
				{noreply, State}
		end
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;
handle_info({udp, Fd, _Ip, _Port, _Msg}, #state{fd = Fd, started = true, weak = false} = State) ->
	inet:setopts(Fd, [{active, once}]),
	?ERR("Disallow data from strange source", []),
	{noreply, State};

handle_info({udp, Fd, Ip, Port, Msg}, #state{fd = Fd, parent = Parent, started = false, neighbour = Neighbour} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		{ok, Rtp} = rtp:decode(Msg),
		gen_server:cast(Parent, {start, self()}),
		gen_server:cast(Neighbour, {rtp, Rtp}),

		% Initial SSRC setup
		% Note - it could change during call w/o warning so beware
		{noreply, State#state{ssrc = Rtp#rtp.ssrc, started = true, ipf = Ip, portf = Port, ipt = Ip, portt = Port, lastseen = now(), alive = true}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;

handle_info(interim_update, #state{started = false} = State) ->
	% Discard - we didn't start yet
	?WARN("RTP_SOCKET: discard interim_update (no other side)~n", []),
	{noreply, State};
handle_info(interim_update, #state{parent = Parent, alive = true} = State) ->
	?INFO("RTP_SOCKET: send interim_update~n", []),
	gen_server:cast(Parent, {interim_update, self()}),
	{noreply, State#state{alive = false}};
handle_info(interim_update, #state{alive = false} = State) ->
	?WARN("RTP_SOCKET: discard interim_update (timeout)~n", []),
	{stop, timeout, State};

%-ifdef(ENABLE_RTCP).

handle_info({udp, Fd, Ip, Port, Msg}, #state{rtcp = Fd, parent = Parent, started1 = true, weak = true} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		{ok, Rtcp} = rtcp:decode(Msg),
		% FIXME Do something with RTCP before sending it further
		gen_server:cast(Parent, {rtcp, Rtcp, self()}),
		{noreply, State#state{ipt1 = Ip, portt1 = Port}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;

handle_info({udp, Fd, Ip, Port, Msg}, #state{rtcp = Fd, parent = Parent, started1 = true, weak = false, ipf1 = Ip, portf1 = Port} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		{ok, Rtcp} = rtcp:decode(Msg),
		% FIXME Do something with RTCP before sending it further
		gen_server:cast(Parent, {rtcp, Rtcp, self()}),
		{noreply, State}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;

handle_info({udp, Fd, _Ip, _Port, _Msg}, #state{rtcp = Fd, started1 = true, weak = false} = State) ->
	inet:setopts(Fd, [{active, once}]),
	?ERR("Disallow data from strange source", []),
	{noreply, State};

handle_info({udp, Fd, Ip, Port, Msg}, #state{rtcp = Fd, parent = Parent, started1 = false} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		{ok, Rtcp} = rtcp:decode(Msg),
		% FIXME Do something with RTCP before sending it further
		gen_server:cast(Parent, {rtcp, Rtcp, self()}),
		{noreply, State#state{started1 = true, ipf1 = Ip, portf1 = Port, ipt1 = Ip, portt1 = Port}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end.

%-else.

%handle_info(_Info, State) ->
%	{noreply, State}.

%-endif.

%%
%% Private functions
%%

transcode(#rtp{payload_type = OldPayloadType, payload = Payload} = Rtp, {PayloadType, Encoder}, Codecs) ->
	Decoder = proplists:get_value(OldPayloadType, Codecs),
	case Decoder of
		passthrough -> {rtp, Rtp};
		undefined -> {rtp, Rtp};
		_ ->
			{ok, RawData} = codec:decode(Decoder, Payload),
			{ok, NewPayload} = codec:encode(Encoder, RawData),
			{rtp, Rtp#rtp{payload_type = PayloadType, payload = NewPayload}}
	end;
transcode(Pkts, _, _) ->
	{rtp, Pkts}.

ensure_ssrc(_, []) ->
	true;
ensure_ssrc(SSRC, #rtp{ssrc = SSRC}) ->
	true;
ensure_ssrc(SSRC, [#rtp{ssrc = SSRC}|Rest]) ->
	ensure_ssrc(SSRC, Rest);
ensure_ssrc(_, _) ->
	false.
