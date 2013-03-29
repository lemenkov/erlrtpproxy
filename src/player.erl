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

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include_lib("rtplib/include/rtp.hrl").

-record(state, {
		subscriber,
		tref,
		marker = 1,
		ssrc,
		data,
		type,
		ssize,
		sn = 0,
		repeats,
		rebuildrtp,
		starttime
	}
).


init([Subcriber, [CodecInfo | _], FN, Playcount]) ->
	process_flag(trap_exit, true),

	{FileExt, Type, FrameLength, Clock} = case CodecInfo of
		{'PCMU', _, _} -> {".pcmu", ?RTP_PAYLOAD_PCMU, 160, 20};
		{'PCMA', _, _} -> {".pcma", ?RTP_PAYLOAD_PCMA, 160, 20};
		{'GSM', _, _} -> {".gsm", ?RTP_PAYLOAD_GSM, 33, 20};
		{'G729', _, _} -> {".g729", ?RTP_PAYLOAD_G729, 20, 20};
		_ -> throw({error, playback_codec_unsupported})
	end,

	{ok, TRef} = timer:send_interval(Clock, send),
	Filename = case FN of
		[$d, $e, $f, $a, $u, $l, $t | _] -> "/tmp/" ++ FN ++ FileExt;
		_ -> FN ++ FileExt
	end,
	{ok, {Fd, Size}} = gen_server:call(storage, {get, Filename}),
	error_logger:info_msg("player: ~p - started to play \"~s\", ~b times~n", [self(), Filename ++ FileExt, Playcount]),
	{ok, #state{
			subscriber = Subcriber,
			tref	= TRef,
			ssrc	= random:uniform(2 bsl 31),
			% Count total number of frames and cut off some weird bytes at the end (if any) - we need only whole frames
			data	= {Fd, Size div FrameLength},
			type	= Type,
			ssize	= FrameLength,
			repeats = Playcount,
			starttime = begin {MegaSecs, Secs, _} = os:timestamp(), MegaSecs*1000000000 + Secs*1000  end
		}
	}.

handle_call(Call, _From,  State) ->
	error_logger:error_msg("player: ~p - unmatched call [~p]", [self(), Call]),
	{stop, {error, {unknown_call, Call}}, State}.

handle_cast(Cast, State) ->
	error_logger:error_msg("player: ~p - unmatched cast [~p]", [self(), Cast]),
	{stop, {error, {unknown_cast, Cast}}, State}.

handle_info(send, #state{subscriber = Subscriber, marker = Marker, sn = SequenceNumber, type = Type, ssize = FrameLength, ssrc = SSRC, data = {Fd, NFrames}, starttime = ST} = State) ->
	Position = FrameLength * (SequenceNumber rem NFrames),
	{ok, Payload} = file:pread(Fd, Position, FrameLength),
	Pkt = #rtp{
			padding = 0,
			marker = Marker,
			payload_type = Type,
			sequence_number = SequenceNumber,
			timestamp = rtp_utils:mktimestamp(Type, ST),
			ssrc = SSRC,
			csrcs = [],
			extension = null,
			payload = Payload
		},
	gen_server:cast(Subscriber, {Pkt, null, null}),
	{noreply, State#state{marker = 0, sn = SequenceNumber + 1}};

handle_info(Info, State) ->
	error_logger:error_msg("player: ~p - unmatched info [~p]", [self(), Info]),
	{stop, {error, {unknown_info, Info}}, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, #state{tref = TRef}) ->
	{memory, Bytes} = erlang:process_info(self(), memory),
	timer:cancel(TRef),
	error_logger:info_msg("player: ~p - terminated due to reason [~p] (allocated ~b bytes)", [self(), Reason, Bytes]).
