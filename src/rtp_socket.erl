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

% Milliseconds
-define(RTP_TIME_TO_LIVE, 150000).
% Milliseconds
-define(INTERIM_UPDATE, 30000).

-record(state, {
		parent,
		fd,
		rtcp,
		transport,
		proto,
		ipf,
		portf,
		ipt,
		portt,
		started,
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

init ([Parent, Transport, Params] = InVals) ->
	process_flag(trap_exit, true),
	% FIXME allow explicitly set address (IP and Port) for both RTP and RTCP sockets
	SockParams = proplists:get_value(sockparams, Params, []),
	% FIXME open socket according Transport - don't hardcode UDP
	Direction = proplists:get_value(direction, Params),
	IsIpv6 = proplists:get_value(ipv6, Params, false),
	{Fd0, Fd1} = rtpproxy_utils:get_fd_pair({Direction, IsIpv6}),

	{ok, TRef} = timer:send_interval(?INTERIM_UPDATE, interim_update),

	Weak = proplists:get_value(weak, Params, false),
	Symmetric = proplists:get_value(symmetric, Params, true),
	TryTranscode = proplists:get_value(transcode, Params, null),

	{Codecs, Transcode} = case TryTranscode of
		null -> {[], null};
		_ ->
			CodecsVals = proplists:get_value(codecs, Params, null),
			C1 = run_codecs(CodecsVals),
			T1 = case TryTranscode of
				{'PCMU',8000,1} -> {?RTP_PAYLOAD_PCMU, proplists:get_value(?RTP_PAYLOAD_PCMU,C1)};
				{'GSM',8000,1} -> {?RTP_PAYLOAD_GSM, proplists:get_value(?RTP_PAYLOAD_GSM,C1)};
				{'PCMA',8000,1} -> {?RTP_PAYLOAD_PCMA, proplists:get_value(?RTP_PAYLOAD_PCMA,C1)};
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

	% Get probable IP and port
	{Ip, Port} = proplists:get_value(rtp, Params, {null, null}),

	?ERR("INIT: ~p", [InVals]),
	{ok, RtcpPid} = rtcp_socket:start_link([self(), Fd1, udp, Params]),
	gen_udp:controlling_process(Fd1, RtcpPid),

	{ok, {I0, P0}} = inet:sockname(Fd0),
	{ok, {I1, P1}} = inet:sockname(Fd1),

	gen_server:cast(Parent, {started, self(), {I0, P0}, {I1, P1}}),

	{ok, #state{
			parent = Parent,
			fd = Fd0,
			rtcp = RtcpPid,
			transport = Transport,
			ipf = null,
			portf = null,
			ipt = Ip,
			portt = Port,
			started = false,
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
handle_cast({rtp, Pkts}, #state{fd = Fd, ipt = Ip, portt = Port, transcode = null} = State) ->
	Msg = rtp:encode(Pkts),
	% FIXME use Transport
	gen_udp:send(Fd, Ip, Port, Msg),
	{noreply, State};
handle_cast({rtp, Pkts}, #state{fd = Fd, ipt = Ip, portt = Port, transcode = Transcode, codecs = Codecs} = State) ->
	{rtp, Pkts2} = transcode(Pkts, Transcode, Codecs),
	Msg = rtp:encode(Pkts2),
	% FIXME use Transport
	gen_udp:send(Fd, Ip, Port, Msg),
	{noreply, State};

handle_cast({rtcp, Pkts, RtcpPid}, #state{parent = Parent, rtcp = RtcpPid} = State) ->
	% FIXME Do something with RTCP before sending it further
	gen_server:cast(Parent, {rtcp, Pkts, self()}),
	{noreply, State};

handle_cast({rtcp, Pkts, Parent}, #state{parent = Parent, rtcp = RtcpPid} = State) ->
	% FIXME Do something with RTCP before sending it further
	gen_server:cast(RtcpPid, {rtcp, Pkts}),
	{noreply, State};

handle_cast({update, Params}, State) ->
	Weak = proplists:get_value(weak, Params, false),
	Symmetric = proplists:get_value(symmetric, Params, true),
	Transcode = proplists:get_value(transcode, Params, null),

	% Get probable IP and port
	Ip = proplists:get_value(ip, Params, null),
	Port = proplists:get_value(port, Params, null),

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
					weak = Weak,
					symmetric = Symmetric
				}
			}
	end;

handle_cast({neighbour, Pid}, State) when is_pid(Pid) ->
	{noreply, State#state{neighbour = Pid}};

handle_cast(stop, #state{tref = TRef} = State) ->
	% Run 30-second Timer instead
	% We shouldn't stop right here. Instead we need to capture all
	% remaining packets if any.
	timer:cancel(TRef),
	{ok, TRef2} = timer:send_interval(?INTERIM_UPDATE, stop),
	{noreply, State#state{tref = TRef2}}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, #state{parent = Parent, fd = Fd, transport = Transport, tref = TRef, codecs = Codecs}) ->
	timer:cancel(TRef),
	Type = case Transport of
		udp -> gen_udp;
		tcp -> gen_tcp;
		sctp -> gen_sctp
	end,
	Type:close(Fd),
	lists:foreach(fun
			({_, passthrough}) -> ok;
			({_, Codec}) -> codec:close(Codec)
		end, Codecs),
	ok.

handle_info({udp, Fd, Ip, Port, Msg}, #state{fd = Fd, parent = Parent, started = true, weak = true, symmetric = Symmetric, neighbour = Neighbour} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		{ok, Pkts} = rtp:decode(Msg),
		process_data(rtp, Pkts, Parent, Neighbour),
		{noreply, State#state{ipt = Ip, portt = Port, lastseen = now(), alive = true}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;

handle_info({udp, Fd, Ip, Port, Msg}, #state{fd = Fd, parent = Parent, started = true, weak = false, ipf = Ip, portf = Port, neighbour = Neighbour} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		{ok, Pkts} = rtp:decode(Msg),
		process_data(rtp, Pkts, Parent, Neighbour),
		{noreply, State#state{lastseen = now(), alive = true}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;
handle_info({udp, Fd, _Ip, _Port, _Msg}, #state{fd = Fd, started = true, weak = false} = State) ->
	inet:setopts(Fd, [{active, once}]),
	?ERR("Disallow data from strange source", []),
	{noreply, State};

handle_info({udp, Fd, Ip, Port, Msg}, #state{parent = Parent, started = false, neighbour = Neighbour} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		{ok, Pkts} = rtp:decode(Msg),
		gen_server:cast(Parent, {start, self()}),
		process_data(rtp, Pkts, Parent, Neighbour),

		{noreply, State#state{started = true, ipf = Ip, portf = Port, lastseen = now(), alive = true}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;

handle_info(ping, State) ->
	{noreply, State};

handle_info(stop, State) ->
	% Final stop
	{stop, stop, State};

handle_info(interim_update, #state{started = false} = State) ->
	% Discard - we didn't start yet
	{noreply, State};
handle_info(interim_update, #state{parent = Parent, alive = true} = State) ->
	gen_server:cast(Parent, {interim_update, self()}),
	{noreply, State#state{alive = false}};
handle_info(interim_update, #state{alive = false} = State) ->
	{stop, timeout, State};

handle_info({'EXIT', Pid, Reason}, #state{rtcp = Pid} = State) ->
	?ERR("RTCP socket died: ~p", [Reason]),
	{noreply, State}.

%%
%% Private functions
%%

process_data(rtp, Pkts, Parent, Neighbour) ->
	gen_server:cast(Neighbour, {rtp, Pkts});
process_data(rtcp, Pkts, Parent, Neighbour) ->
	gen_server:cast(Parent, {rtcp, Pkts, self(), Neighbour}).

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

run_codecs(CodecsVals) ->
	run_codecs(CodecsVals, []).
run_codecs([], Ret) ->
	Ret;
run_codecs([{'PCMU',8000,1} | Rest], Ret) ->
	{ok, Codec} = codec:start_link([?RTP_PAYLOAD_PCMU, self()]),
	run_codecs(Rest, Ret ++ [{?RTP_PAYLOAD_PCMU, Codec}]);
run_codecs([{'PCMA',8000,1} | Rest], Ret) ->
	{ok, Codec} = codec:start_link([?RTP_PAYLOAD_PCMA, self()]),
	run_codecs(Rest, Ret ++ [{?RTP_PAYLOAD_PCMA, Codec}]);
run_codecs([{'GSM',8000,1} | Rest], Ret) ->
	{ok, Codec} = codec:start_link([?RTP_PAYLOAD_GSM, self()]),
	run_codecs(Rest, Ret ++ [{?RTP_PAYLOAD_GSM, Codec}]);
run_codecs([CodecVal | Rest], Ret) ->
	run_codecs(Rest, Ret ++ [{CodecVal, passthrough}]).
