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

-export([start/3]).
-export([send_rtp/2]).

-include("common.hrl").

% http://en.wikipedia.org/wiki/RTP_Audio_Video_Profiles
-define(RTP_PCMU, 0).
-define(RTP_GSM, 3).
-define(RTP_G723, 4).
-define(RTP_PCMA, 8).
-define(RTP_CN, 13).
-define(RTP_G729, 18).
-define(RTP_TSE, 100).
-define(RTP_TSE_CISCO, 101).

-record(state, {type,
		clock,
		framelength,
		sequencenumber=1,
		timestamp,
		marker=1,
		time,
		ssrc,
		pos=0,
		startfrom,
		myfd,
		addr}).

start (Filename, PayloadTypeStr, Addr) ->
	process_flag(trap_exit, true),
	{Type, FrameLength, Clock} = case PayloadTypeStr of
		"0PCMU/8000" -> {?RTP_PCMU, 160, 8000};
		"101telephone-event/8000" -> {?RTP_PCMU, 160, 8000};
		"8PCMA/8000" -> {?RTP_PCMA, 160, 8000};
		_ -> {?RTP_G729, 10, 0}
	end,
	{Fd, Ip, Port, MyFd} = case Addr of
		{null, Ip1, Port1} ->
			{ok, Fd1} = gen_udp:open(0, [binary, {active, true}]),
			{Fd1, Ip1, Port1, true};
		{Fd1, Ip1, Port1} ->
			{Fd1, Ip1, Port1, false}
	end,
	Fn = io_lib:format("~s.~b", [Filename, Type]),
	case file:read_file(Fn) of
		{ok, RtpData} ->
			State = #state{	time=round(1000 / (Clock / FrameLength)),
					type=Type,
					clock=Clock,
					framelength=FrameLength,
					timestamp=FrameLength,
					ssrc=random:uniform(2 bsl 31),
					startfrom=now(),
					myfd=MyFd,
					addr={Fd, Ip, Port}},
			{ok, spawn(node(), player, send_rtp, [RtpData, State])};
		{error, Reason} ->
			{error, Reason}
	end.

send_rtp (RtpData, State) ->
	% TODO add more variants (probably when makeann should be able to encode new formats)
	% TODO Move to  start (...)

	Pos = case State#state.pos == size(RtpData) of
		true -> 0;
		_ -> State#state.pos
	end,

	PayloadLength =	if
		Pos + State#state.framelength > size (RtpData) -> size(RtpData) - Pos;
		true -> State#state.framelength
	end,

	<<_Head:Pos/binary, Payload:PayloadLength/binary, _Tail/binary>> = RtpData,

	{Fd, Ip, Port} = State#state.addr,

	gen_udp:send(Fd, Ip, Port,
		<<
			2:2,
			0:1,
			0:1,
			0:4,
			(State#state.marker):1,
			(State#state.type):7,
			(State#state.sequencenumber):16,
			(State#state.timestamp):32,
			(State#state.ssrc):32,
			Payload:PayloadLength/binary
		>>),
	{MegaSecs0, Secs0, MicroSecs0} = State#state.startfrom,
	{MegaSecs, Secs, MicroSecs} = now(),
	Wait = case round((State#state.sequencenumber * State#state.time) - (((MegaSecs - MegaSecs0) * 1000000 + (Secs - Secs0)) * 1000 + (MicroSecs - MicroSecs0)/1000)) of
		Interval when Interval > 0, Interval < State#state.time ->
			Interval;
		_ ->
			19
	end,

	receive
		Something ->
			?PRINT("listener RECEIVED [~p]!~n", [Something]),
			if
				State#state.myfd == true -> gen_udp:close (Fd);
				true -> ok
			end,
			gen_server:cast({global, rtpproxy}, {call_terminated, {self(), Something}})
	after Wait  ->
		send_rtp(RtpData, State#state{
					timestamp=State#state.framelength + State#state.timestamp,
					marker=0,
					pos=Pos+PayloadLength,
					sequencenumber=State#state.sequencenumber + 1})
	end.

