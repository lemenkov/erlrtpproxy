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
						DecodeSdesItems = fun (F5) ->
							fun	({<<>>, Items}) ->
									{Items, <<>>};
								({<<?SDES_CNAME:8, L:8, 16#DE:8, 16#AD:8, V:L/binary, Tail/binary>>, Items}) ->
									error_logger:warning_msg("SDES_CNAME, padding from AddPac (0xDE, 0xAD)~n", []),
									F5({Tail, Items#sdes_items{cname=binary_to_list(V)}});
								({<<?SDES_CNAME:8, L:8, 16#79:8, 16#00:8, V:L/binary, Tail/binary>>, Items}) ->
									error_logger:warning_msg("SDES_CNAME, padding from AddPac (0x79, 0x00)~n", []),
									F5({Tail, Items#sdes_items{cname=binary_to_list(V)}});
								({<<?SDES_CNAME:8, L:8, 16#00:8, 16#00:8, V:L/binary, Tail/binary>>, Items}) ->
									error_logger:warning_msg("SDES_CNAME, padding from AddPac (0x00, 0x00)~n", []),
									F5({Tail, Items#sdes_items{cname=binary_to_list(V)}});
								({<<?SDES_CNAME:8, L:8, 16#00:8, 16#07:8, V:L/binary, Tail/binary>>, Items}) ->
									error_logger:warning_msg("SDES_CNAME, padding from AddPac (0x00, 0x07)~n", []),
									F5({Tail, Items#sdes_items{cname=binary_to_list(V)}});
								({<<?SDES_CNAME:8, L:8, 16#00:8, 16#05:8, V:L/binary, Tail/binary>>, Items}) ->
									error_logger:warning_msg("SDES_CNAME, padding from AddPac (0x00, 0x05)~n", []),
									F5({Tail, Items#sdes_items{cname=binary_to_list(V)}});
								({<<?SDES_CNAME:8, L:8, 16#00:8, 16#0D:8, V:L/binary, Tail/binary>>, Items}) ->
									error_logger:warning_msg("SDES_CNAME, padding from AddPac (0x00, 0x0D)~n", []),
									F5({Tail, Items#sdes_items{cname=binary_to_list(V)}});
								({<<?SDES_CNAME:8, L:8, V:L/binary, Tail/binary>>, Items}) ->
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
									R = 8*(size(Tail) rem 4),
									<<_:R, NextSDESItems/binary>> = Tail,
									{Items, NextSDESItems};
								({<<_:8, L:8, _:L/binary, Tail/binary>>, Items}) ->
									F5({Tail, Items})
							end
						end,
						(y:y(fun (F) ->
							fun	({<<>>, _, Result}) -> #sdes{list=Result};
								({Padding, 0, Result}) ->
									error_logger:warning_msg("SDES padding [~p]~n", [Padding]),
									#sdes{list=Result};
								({<<SSRC1:32, SDESItems/binary>>, SC, Result}) when SC>0 ->
									{Items, Rest} = (y:y(DecodeSdesItems))({SDESItems, #sdes_items{ssrc=SSRC1}}),
									F({Rest, SC-1, Result ++ [Items]})
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

