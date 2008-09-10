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

-record(state, {payloadtype=null,
		payloadlimit=0,
		sequencenumber=0,
		timestamp=0,
		time=0,
		ssrc=0,
		pos=0,
		myfd=false,
		addr=null}).

start (Filename, PayloadTypeStr, Addr) ->
	process_flag(trap_exit, true),
	{PayloadType, PayloadLength, PayloadLimit} = case PayloadTypeStr of
		"0PCMU/8000" -> {?RTP_PCMU, 160, 8000};
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
	case file:read_file(Filename) of
		{ok, RtpData} ->
			{MegaSecs, Secs, MicroSecs} = now(),
			Timestamp = (MegaSecs * 1000000 +  Secs) * 1000000 + MicroSecs,
			State = #state{	time=round(773 * PayloadLength / PayloadLimit),
					payloadtype=PayloadType,
					payloadlimit=PayloadLimit,
					timestamp=Timestamp,
					ssrc=random:uniform(2 bsl 31),
					myfd=MyFd,
					addr={Fd, Ip, Port}},
			{ok, spawn(node(), player, send_rtp, [RtpData, State])};
		{error, Reason} ->
			{error, Reason}
	end.

difftime({MegaSecs0, Secs0, MicroSecs0}) ->
	{MegaSecs1, Secs1, MicroSecs1} = now(),
	round((((MegaSecs1 - MegaSecs0) * 1000000 + (Secs1 - Secs0)) * 1000000 + (MicroSecs1 - MicroSecs0))/1000).

send_rtp (RtpData, State) ->
	% TODO add more variants (probably when makeann should be able to encode new formats)
	% TODO Move to  start (...)
%	?PRINT("listener begin! State[~w], sent ~w and remains ~w bytes.~n", [State, State#state.pos, size(RtpData)]),
	Begin = now(),

	Pos = case State#state.pos == size(RtpData) of
		true -> 0;
		_ -> State#state.pos
	end,

	PayloadLength = case State#state.payloadtype of
		?RTP_PCMU ->
			if
				Pos + 160 > size (RtpData) -> size(RtpData) - Pos;
				true -> 160
			end;
		?RTP_PCMA ->
			if
				Pos + 160 > size (RtpData) -> size(RtpData) - Pos;
				true -> size(RtpData)
			end;
		?RTP_G729 -> 10;
		?RTP_G723 -> 24;
		?RTP_GSM -> 33;
		_ -> 0
	end,

%	?PRINT("listener 1!~n", []),
%	?PRINT("player pos ~w from ~w!~n", [Pos, size(RtpData)]),
	<<_Head:Pos/binary, Payload:PayloadLength/binary, _Tail/binary>> = RtpData,

%	{MegaSecs, Secs, MicroSecs} = now(),
%	Timestamp = (MegaSecs * 1000000 +  Secs) * 1000000 + MicroSecs - State#state.timestamp,
	Timestamp =  160 + State#state.timestamp,

	{Fd, Ip, Port} = State#state.addr,

%	?PRINT("listener 3!~n", []),
	gen_udp:send(Fd, Ip, Port, <<2:2, 0:1, 0:1, 0:4, 0:1, (State#state.payloadtype):7, (State#state.sequencenumber):16, Timestamp:32, (State#state.ssrc):32, Payload:PayloadLength/binary>>),

	Wait = State#state.time - difftime(Begin),

%	?PRINT("listener waits for ~w msec from ~w~n", [Wait, State#state.time]),
	receive
		Something ->
			?PRINT("listener RECEIVED [~p]!~n", [Something]),
			if
				State#state.myfd == true -> gen_udp:close (Fd);
				true -> ok
			end,
			gen_server:cast({global, rtpproxy}, {call_terminated, {self(), Something}})
%	after State#state.time  ->
	after Wait  ->
%		?PRINT("listener 5!~n", []),
		send_rtp(RtpData, State#state{timestamp=Timestamp, pos=Pos+PayloadLength, sequencenumber=State#state.sequencenumber + 1})
	end.

