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
-export([send_rtp/8]).

-include("common.hrl").

-define(RTP_PCMU, 0).
-define(RTP_GSM, 3).
-define(RTP_G723, 4).
-define(RTP_PCMA, 8).
-define(RTP_CN, 13).
-define(RTP_G729, 18).
-define(RTP_TSE, 100).
-define(RTP_TSE_CISCO, 101).

start (Filename, PayloadType, IpAddrs) ->
	case file:read_file(Filename) of
		{ok, RtpData} ->
			{ok, Fd} = gen_udp:open(0, [binary, {active, true}]),
			{MegaSecs, Secs, MicroSecs} = now(),
			Timestamp = (MegaSecs * 1000000 +  Secs) * 1000000 + MicroSecs,
			{ok, spawn(player, send_rtp, [RtpData, Fd, PayloadType, Timestamp, random:uniform(2 bsl 31), IpAddrs, <<"">>])};
		{error, Reason} ->
			{error, Reason}
	end.

send_rtp (<<"">>, Fd, PayloadType, SequenceNumber, TimestampBase, SSRC, IpAddrs, Sent) ->
	send_rtp (Sent, Fd, PayloadType, SequenceNumber, TimestampBase, SSRC, IpAddrs, <<"">>);

send_rtp (RtpData, Fd, PayloadType, SequenceNumber, TimestampBase, SSRC, IpAddrs, Sent) ->
	% TODO add more variants (probably when makeann should be able to encode new formats)
	PayloadLength = case PayloadType of
		<<?RTP_PCMU:7>> -> 160;
		<<?RTP_PCMA:7>> -> 160;
		<<?RTP_G729:7>> -> 10;
		<<?RTP_G723:7>> -> 24;
		<<?RTP_GSM:7>> -> 33;
		_ -> 0
	end,

	<<Payload:PayloadLength/binary, Rest/binary>> = RtpData,
	{MegaSecs, Secs, MicroSecs} = now(),
	Timestamp = (MegaSecs * 1000000 +  Secs) * 1000000 + MicroSecs - TimestampBase,

	% TODO
	[{{Ip1, Port1}, {Ip2, Port2}}|Other] = IpAddrs,
	gen_udp:send(Fd, Ip1, Port1, <<2:2, 0:1, 0:1, 0:4, 0:1, PayloadType:7, SequenceNumber:16, Timestamp:32, SSRC:32, Payload:PayloadLength/binary>>),
	gen_udp:send(Fd, Ip2, Port2, <<2:2, 0:1, 0:1, 0:4, 0:1, PayloadType:7, SequenceNumber:16, Timestamp:32, SSRC:32, Payload:PayloadLength/binary>>),

	receive
		_ ->
			gen_udp:close(Fd),
			io:format("listener RECEIVED!~n")
	after 0 ->
		send_rtp(Rest, Fd, PayloadType, SequenceNumber + 1, TimestampBase, SSRC, IpAddrs, <<Sent/binary, Payload:PayloadLength/binary>>)
	end.

