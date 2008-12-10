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

-module(rtcp).
-author('lemenkov@gmail.com').

-export([decode/1]).

-include("rtcp.hrl").
-include("common.hrl").

decode(Data) ->
	DecodeRtcp = fun(F1) ->
		fun	({<<>>, Rtcp}) -> Rtcp;
			({<<Ver:2, Padding:1, RC:5, PacketType:8, Length:16, Tail1/binary>>, OldRtcp}) ->
				Length1 = Length*4,
				<<Payload:Length1/binary, Next/binary>> = Tail1,
				Rtcp = case PacketType of
					?RTCP_SR ->
						<<SSRC:32, NTPSec:32, NTPFrac:32, TimeStamp:32, Packets:32, Octets:32, ReportBlocks/binary>> = Payload,
						DecodeRblocks = fun(F3) ->
							fun	({<<>>, 0, Result}) -> Result;
								({SomethingStrange, 0, Result}) ->
									?ERR("SR strange Rblock [~p]~n", [SomethingStrange]),
									Result;
								({<<SSRC1:32, FL:8, CNPL:24/signed, EHSNR:32, IJ:32, LSR:32, DLSR:32, Rest/binary>>, RC1, Result}) ->
									F3({Rest, RC1-1, Result ++ [#rblock{ssrc=SSRC1, fraction=FL, lost=CNPL, last_seq=EHSNR, jitter=IJ, lsr=LSR, dlsr=DLSR}]})
							end
						end,
						Ntp2Now = fun(NTPSec1, NTPFrac1) ->
							MegaSecs = NTPSec1 div 1000000,
							Secs = NTPSec1 rem 1000000,
							R = lists:foldl(fun(X, Acc) -> Acc + ((NTPFrac1 bsr (X-1)) band 1)/(2 bsl (32-X)) end, 0, lists:seq(1, 32)),
							MicroSecs = trunc(1000000*R),
							{MegaSecs, Secs, MicroSecs}
						end,
						#sr{ssrc=SSRC, ntp=Ntp2Now(NTPSec, NTPFrac), timestamp=TimeStamp, packets=Packets, octets=Octets, rblocks=(utils:y(DecodeRblocks))({ReportBlocks, RC, []})};
					?RTCP_RR ->
						<<SSRC:32, ReportBlocks/binary>> = Payload,
						DecodeRblocks = fun(F3) ->
							fun	({<<>>, 0, Result}) -> Result;
								({SomethingStrange, 0, Result}) ->
									?ERR("RR strange Rblock [~p]~n", [SomethingStrange]),
									Result;
								({<<SSRC1:32, FL:8, CNPL:24/signed, EHSNR:32, IJ:32, LSR:32, DLSR:32, Rest/binary>>, RC1, Result}) ->
									F3({Rest, RC1-1, Result ++ [#rblock{ssrc=SSRC1, fraction=FL, lost=CNPL, last_seq=EHSNR, jitter=IJ, lsr=LSR, dlsr=DLSR}]})
							end
						end,
						#rr{ssrc=SSRC, rblocks=(utils:y(DecodeRblocks))({ReportBlocks, RC, []})};
					?RTCP_SDES ->
						DecodeSdesItems = fun (F5) ->
							fun	({<<?SDES_CNAME:8, L:8, V:L/binary, Tail/binary>>, Items}) ->
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
						DecodeSdes = fun (F4) ->
							fun	({<<>>, 0, Result}) -> #sdes{list=Result};
								({SomethingStrange, 0, Result}) ->
									?ERR("SDES strange padding [~p]~n", [SomethingStrange]),
									#sdes{list=Result};
								({<<SSRC1:32, SDESItems/binary>>, SC, Result}) when SC>0 ->
									{Items, Rest} = (utils:y(DecodeSdesItems))({SDESItems, #sdes_items{ssrc=SSRC1}}),
									F4({Rest, SC-1, Result ++ [Items]})
							end
						end,
						(utils:y(DecodeSdes))({Payload, RC, []});
					?RTCP_BYE ->
						DecodeBye = fun(F2) ->
							fun	({<<>>, 0, Ret}) ->
									#bye{params=Ret};
								({<<L:8, Text:L/binary, _/binary>>, 0, Ret}) ->
									#bye{message=binary_to_list(Text), params=Ret};
								({<<SSRC:32, Tail/binary>>, RC1, Ret}) when RC1>0 ->
									F2({Tail, RC1-1, Ret ++ [SSRC]})
							end
						end,
						(utils:y(DecodeBye))({Payload, RC, []});
					_ ->
						{error, unknown_type}
				end,
				F1({Next, OldRtcp ++ [Rtcp]})
		end
	end,
	(utils:y(DecodeRtcp))({Data, []}).

