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

-module(player).
-author('lemenkov@gmail.com').

-behaviour(gen_server).

-export([start/4]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include_lib("rtplib/include/rtp.hrl").

-record(state, {
		callid,
		mediaid,
		tag,
		tref,
		ssrc,
		data,
		type,
		ssize,
		sn = 0,
		repeats = 0,
		rebuildrtp,
		starttime
	}
).


start(CallId, MediaId, Tag, PayloadInfo) ->
	gen_server:start(?MODULE, [CallId, MediaId, Tag, PayloadInfo], []).

init([CallId, MediaId, Tag, PayloadInfo]) ->
	% Register itself
	gproc:reg({n,l,{player, CallId, MediaId, Tag}}),

	% How many times we should playbak (FIXME not used for now)
	Playcount = proplists:get_value(playcount, PayloadInfo, 0),
	% We need a codec
	[CodecInfo | _] = proplists:get_value(codecs, PayloadInfo),
	Filename = binary_to_list(proplists:get_value(filename, PayloadInfo, <<"default">>)),
	{FileExt, Type, FrameLength, Clock} = case CodecInfo of
		{'PCMU', _, _} -> {".pcmu", ?RTP_PAYLOAD_PCMU, 160, 20};
		{'PCMA', _, _} -> {".pcma", ?RTP_PAYLOAD_PCMA, 160, 20};
		{'GSM', _, _} -> {".gsm", ?RTP_PAYLOAD_GSM, 33, 20};
		{'G729', _, _} -> {".g729", ?RTP_PAYLOAD_G729, 20, 20};
		_ -> throw({error, playback_codec_unsupported})
	end,

	{ok, TRef} = timer:send_interval(Clock, send),
	{ok, {Fd, Size}} = gen_server:call(storage, {get, "/tmp/" ++ Filename ++ FileExt}),
	{ok, #state{
			callid	= CallId,
			mediaid = MediaId,
			tag	= Tag,
			tref	= TRef,
			ssrc	= random:uniform(2 bsl 31),
			data	= {Fd, Size},
			type	= Type,
			ssize	= FrameLength,
			repeats = Playcount,
			starttime = begin {MegaSecs, Secs, _} = os:timestamp(), MegaSecs*1000000000 + Secs*1000  end
		}
	}.

handle_call(Call, _From,  State) ->
	error_logger:warning_msg("Unmatched call [~p]", [Call]),
	{reply,{error,unknown_call},State}.

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(Other, State) ->
	error_logger:warning_msg("Unmatched cast [~p]", [Other]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, #state{tref = TRef}) ->
	{memory, Bytes} = erlang:process_info(self(), memory),
	timer:cancel(TRef),
	error_logger:info_msg("player terminated due to reason [~p] (allocated ~b bytes)", [Reason, Bytes]).

handle_info(send, #state{callid = C, mediaid = M, tag = T, sn = SequenceNumber, type = Type, ssize = FrameLength, ssrc = SSRC, data = {Fd, Size}, starttime = ST} = State) ->
	case gproc:select([{{{n,l,{media, C, M, T}}, '$1', '_'}, [], ['$1']}]) of
		[] ->
			{noreply, State};
		[Pid] ->
			Length = Size - FrameLength,
			P = FrameLength*SequenceNumber,
			Position = case P < Length of
				true -> P;
				_ -> P rem Length
			end,
			{ok, Payload} = file:pread(Fd, Position, FrameLength),
			Timestamp = rtp_utils:mktimestamp(Type, ST),
			Pkt = #rtp{
					padding = 0,
					marker = case SequenceNumber of 0 -> 1; _ -> 0 end,
					payload_type = Type,
					sequence_number = SequenceNumber,
					timestamp = Timestamp,
					ssrc = SSRC,
					csrcs = [],
					extension = null,
					payload = Payload
				},
			gen_server:cast(Pid, {'music-on-hold', Pkt}),
			{noreply, State#state{sn = SequenceNumber + 1}}
	end;

handle_info(Info, State) ->
	error_logger:warning_msg("Unmatched info [~p]", [Info]),
	{noreply, State}.
