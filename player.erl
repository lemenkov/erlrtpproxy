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
-export([send_rtp/3]).

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
		timestampbase=0,
		time=0,
		ssrc=0,
		addrs=null}).

start (Filename, PayloadTypeStr, IpAddrs) ->
	process_flag(trap_exit, true),
	{PayloadType, PayloadLimit} = case PayloadTypeStr of
		"0PCMU/8000" -> {?RTP_PCMU, 8000};
		"8PCMA/8000" -> {?RTP_PCMA, 8000};
		_ -> {?RTP_G729, 0}
	end,

	case file:read_file(Filename) of
		{ok, RtpData} ->
			{MegaSecs, Secs, MicroSecs} = now(),
			TimestampBase = (MegaSecs * 1000000 +  Secs) * 1000000 + MicroSecs,
			State = #state{payloadtype=PayloadType, payloadlimit=PayloadLimit, timestampbase=TimestampBase, ssrc=random:uniform(2 bsl 31), addrs=IpAddrs},
			{ok, spawn(node(), player, send_rtp, [RtpData, State, <<"">>])};
		{error, Reason} ->
			{error, Reason}
	end.

send_rtp (<<"">>, State, Sent) ->
	send_rtp (Sent, State, <<"">>);

send_rtp (RtpData, State, Sent) ->
	% TODO add more variants (probably when makeann should be able to encode new formats)
	% TODO Move to  start (...)
	?PRINT("listener begin! State[~w], sent ~w and remains ~w bytes.~n", [State, size(Sent), size(RtpData)]),
	PayloadLength = case State#state.payloadtype of
		?RTP_PCMU ->
			if
				size (RtpData) > 160 -> 160;
				true -> size(RtpData)
			end;
		?RTP_PCMA ->
			if
				size (RtpData) > 160 -> 160;
				true -> size(RtpData)
			end;
		?RTP_G729 -> 10;
		?RTP_G723 -> 24;
		?RTP_GSM -> 33;
		_ -> 0
	end,

%	?PRINT("listener 1!~n", []),
	<<Payload:PayloadLength/binary, Rest/binary>> = RtpData,

	{MegaSecs, Secs, MicroSecs} = now(),
	Timestamp = (MegaSecs * 1000000 +  Secs) * 1000000 + MicroSecs - State#state.timestampbase,

	% TODO
%	?PRINT("listener 2! ~w~n", [State#state.addrs]),
	[{{Fd1, Ip1, Port1}, {Fd2, Ip2, Port2}}|Other] = State#state.addrs,

%	?PRINT("listener 3!~n", []),
	gen_udp:send(Fd1, Ip1, Port1, <<2:2, 0:1, 0:1, 0:4, 0:1, (State#state.payloadtype):7, (State#state.sequencenumber):16, Timestamp:32, (State#state.ssrc):32, Payload:PayloadLength/binary>>),
	gen_udp:send(Fd2, Ip2, Port2, <<2:2, 0:1, 0:1, 0:4, 0:1, (State#state.payloadtype):7, (State#state.sequencenumber):16, Timestamp:32, (State#state.ssrc):32, Payload:PayloadLength/binary>>),

%	?PRINT("listener 4! wait for ~w msec~n", [State#state.time]),
	receive
		Something ->
			?PRINT("listener RECEIVED [~p]!~n", [Something]),
			gen_server:cast({global, rtpproxy}, {call_terminated, {self(), Something}})
	after State#state.time  ->
%		?PRINT("listener 5!~n", []),
		send_rtp(Rest, State#state{time=round(1000 * PayloadLength / State#state.payloadlimit), sequencenumber=State#state.sequencenumber + 1}, <<Sent/binary, Payload:PayloadLength/binary>>)
	end.

