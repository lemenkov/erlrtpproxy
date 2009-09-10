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

% see this RFC for further details:
% http://www.ietf.org/rfc/rfc3550.txt

-module(rtcp).
-author('lemenkov@gmail.com').

-export([encode/1]).
-export([decode/1]).

-export([version/0]).

-include("rtcp.hrl").

encode(Packets) ->
	% TODO
	ok.

decode(Data) ->
	DecodeRtcp = fun(F1) ->
		fun	({<<>>, DecodedRtcps}) -> DecodedRtcps;
			% Version is always 2
			({<<?RTCP_VERSION:2, PaddingFlag:1, RC:5, PacketType:8, Length:16, Tail1/binary>>, DecodedRtcps}) ->
				Length1 = Length*4,
				<<Payload:Length1/binary, Next/binary>> = Tail1,
				Rtcp = case PacketType of
					?RTCP_SR ->
						<<SSRC:32, NTPSec:32, NTPFrac:32, TimeStamp:32, Packets:32, Octets:32, ReportBlocks/binary>> = Payload,
						DecodeRblocks = fun(F) ->
							fun	({<<>>, _, Result}) -> Result;
								({Padding, 0, Result}) ->
									error_logger:warning_msg("SR padding [~p]~n", [Padding]),
									Result;
								({<<SSRC1:32, FL:8, CNPL:24/signed, EHSNR:32, IJ:32, LSR:32, DLSR:32, Rest/binary>>, RC1, Result}) ->
									F({Rest, RC1-1, Result ++ [#rblock{ssrc=SSRC1, fraction=FL, lost=CNPL, last_seq=EHSNR, jitter=IJ, lsr=LSR, dlsr=DLSR}]})
							end
						end,
						Ntp2Now = fun(NTPSec1, NTPFrac1) ->
							MegaSecs = NTPSec1 div 1000000,
							Secs = NTPSec1 rem 1000000,
							R = lists:foldl(fun(X, Acc) -> Acc + ((NTPFrac1 bsr (X-1)) band 1)/(2 bsl (32-X)) end, 0, lists:seq(1, 32)),
							MicroSecs = trunc(1000000*R),
							{MegaSecs, Secs, MicroSecs}
						end,
						#sr{ssrc=SSRC, ntp=Ntp2Now(NTPSec, NTPFrac), timestamp=TimeStamp, packets=Packets, octets=Octets, rblocks=(y:y(DecodeRblocks))({ReportBlocks, RC, []})};
					?RTCP_RR ->
						<<SSRC:32, ReportBlocks/binary>> = Payload,
						DecodeRblocks = fun(F) ->
							fun	({<<>>, _, Result}) -> Result;
								({Padding, 0, Result}) ->
									error_logger:warning_msg("RR padding [~p]~n", [Padding]),
									Result;
								({<<SSRC1:32, FL:8, CNPL:24/signed, EHSNR:32, IJ:32, LSR:32, DLSR:32, Rest/binary>>, RC1, Result}) ->
									F({Rest, RC1-1, Result ++ [#rblock{ssrc=SSRC1, fraction=FL, lost=CNPL, last_seq=EHSNR, jitter=IJ, lsr=LSR, dlsr=DLSR}]})
							end
						end,
						#rr{ssrc=SSRC, rblocks=(y:y(DecodeRblocks))({ReportBlocks, RC, []})};
					?RTCP_SDES ->
						% There may be RC number of chunks (we call them Chunks), containing of their own SSRC 32-bit identificator
						% and arbitrary number of SDES-items.
						io:format("PaddingFlag ~p, RC ~p, PacketType ~p, Length ~p~n", [PaddingFlag, RC, PacketType, Length1]),

						% Recursively process each chunk and return list of SDES-items
						DecodeSdesItems = fun (F5) ->
								% All items are ItemID:8_bit, Lenght:8_bit, ItemData:Length_bit
							fun	({<<?SDES_CNAME:8, L:8, V:L/binary, Tail/binary>>, Items}) ->
									% AddPac sends us wrongly produced CNAME item (with 2-byte arbitrary padding inserted):
									% <<?SDES_CNAME:8, 19:8, ArbitraryPadding:16, "AddPac VoIP Gateway":(19*8)/binary>>
									% I don't think that we need to fix it.
									F5({Tail, Items#sdes_items{cname=binary_to_list(V)}});
								({<<?SDES_NAME:8, L:8, V:L/binary, Tail/binary>>, Items}) ->
									F5({Tail, Items#sdes_items{name=binary_to_list(V)}});
								({<<?SDES_EMAIL:8, L:8, V:L/binary, Tail/binary>>, Items}) ->
									F5({Tail, Items#sdes_items{email=binary_to_list(V)}});
								({<<?SDES_PHONE:8, L:8, V:L/binary, Tail/binary>>, Items}) ->
									F5({Tail, Items#sdes_items{phone=binary_to_list(V)}});
								({<<?SDES_LOC:8, L:8, V:L/binary, Tail/binary>>, Items}) ->
									F5({Tail, Items#sdes_items{loc=binary_to_list(V)}});
								({<<?SDES_TOOL:8, L:8, V:L/binary, Tail/binary>>, Items}) ->
									F5({Tail, Items#sdes_items{tool=binary_to_list(V)}});
								({<<?SDES_NOTE:8, L:8, V:L/binary, Tail/binary>>, Items}) ->
									F5({Tail, Items#sdes_items{note=binary_to_list(V)}});
								({<<?SDES_PRIV:8, L:8, V:L/binary, Tail/binary>>, Items}) ->
									F5({Tail, Items#sdes_items{priv=V}});
								({<<?SDES_NULL:8, Tail/binary>>, Items}) ->
									% This is NULL terminator
									% LEt's calculate how many bits we need to skip (padding up to 32-bit boundaries)
									R = 8*(size(Tail) rem 4),
									<<_PaddingBits:R, Rest/binary>> = Tail,
									% mark this SDES chunk as null-terminated properly and return
									{Items#sdes_items{eof=true}, Rest};
								({<<_:8, L:8, _:L/binary, Tail/binary>>, Items}) ->
									% unknown SDES item - skip it and proceed to the next one
									F5({Tail, Items});
								({Rest, Items}) ->
									% possibly, next SDES chunk - just stop and return what was already decoded
									{Items, Rest}
							end
						end,

						% Recursively process package for SSRC+SDES chunks
						(y:y(fun (F) ->
							fun
								% Disregard SDES items count (SC) if no data remaining
								% simply construct #sdes{} from the  resulting list of SDES-items (Result)
								% and return
								({<<>>, _SC, Result}) ->
									#sdes{list=Result};

								% SDES may contain padding (should be noted by PaddingFlag)
								% Likewise.
								({Padding, 0, Result}) ->
									error_logger:warning_msg("SDES padding [~p]~n", [Padding]),
									#sdes{list=Result};

								% Each SDES-item followed by their own SSRC value (they are not necessary the same)
								% and the arbitrary raw data
								({<<SSRC1:32, RawData/binary>>, SC, Result}) when SC>0 ->
									{Items, RawDataRest} = (y:y(DecodeSdesItems))({RawData, #sdes_items{ssrc=SSRC1}}),

									% We processing next possible SDES chunk
									% - We decrease SDES count (SC) by one, since we already proccesses one SDES chunk
									% - We added previously decoded and packed into #sdes{} SDES-items to the list of 
									%   already processed SDES chunks
									F({RawDataRest, SC-1, Result ++ [Items]})

							end
						end))({Payload, RC, []});
					?RTCP_BYE ->
						(y:y(fun(F) ->
							fun	({<<>>, 0, Ret}) ->
									#bye{params=Ret};
								({Padding, 0, Ret}) ->
									error_logger:warning_msg("BYE padding [~p]~n", [Padding]),
									#bye{params=Ret};
								({<<L:8, Text:L/binary, _/binary>>, 0, Ret}) ->
									#bye{message=binary_to_list(Text), params=Ret};
								({<<SSRC:32, Tail/binary>>, RC1, Ret}) when RC1>0 ->
									F({Tail, RC1-1, Ret ++ [SSRC]})
							end
						end))({Payload, RC, []});
					_ ->
						{error, unknown_type}
				end,
				F1({Next, DecodedRtcps ++ [Rtcp]})
		end
	end,
	(y:y(DecodeRtcp))({Data, []}).

version() ->
	io:format("version 0.1.2~n").
