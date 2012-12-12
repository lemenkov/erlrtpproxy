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

-module(stats).
-author('lemenkov@gmail.com').

-export([dispatch/1]).

dispatch(Req) ->
	case Req:get(method) of
		'GET' ->
			% Build JSON
			Path = Req:get(path),
			case string:tokens(Path, "/") of
				["html" | Rest] ->
					JSON = "{hello:\"html\"}",
					Req:respond({200, [], JSON});
				["json" | Rest] ->
					Length = length(gproc:lookup_global_properties(media)) div 2,
					Calls = {struct,
						drop_dupes(
							lists:map(
							fun({_,{id,CallId,MediaId}}) ->
								{call, [{callid, CallId}, {mediaid, MediaId}]}
							end,
							gproc:lookup_global_properties(media))
						)
					},
					LocalPhys = {struct,
						lists:map(
							fun({_,{C, M, T, Ip, PortRtp, PortRtcp}}) ->
								{phy, [{callid, C}, {mediaid, M}, {tag, T}, {ip, list_to_binary(inet_parse:ntoa(Ip))}, {rtp, PortRtp}, {rtcp, PortRtcp}]}
							end,
							gproc:lookup_global_properties(local)
						)
					},
					RemotePhys = {struct,
						lists:map(
							fun({_,{C, M, T, Ip, PortRtp, PortRtcp}}) ->
								{phy, [{callid, C}, {mediaid, M}, {tag, T}, {ip, list_to_binary(inet_parse:ntoa(Ip))}, {rtp, PortRtp}, {rtcp, PortRtcp}]}
							end,
							gproc:lookup_global_properties(remote)
						)
					},
					Payloads = {struct,
						lists:map(
							fun({_,{C, M, T, Type}}) ->
								{media, [{callid, C}, {mediaid, M}, {tag, T}, {payload_type, Type}]}
							end,
							gproc:lookup_global_properties(payload_type)
						)
					},
					JSON = mochijson2:encode({struct, [{callnum, Length}, {calls, Calls}, {remotephys, RemotePhys}, {localphys, LocalPhys}, {payloads, Payloads}]}),
					Req:respond({200, [], JSON});
				_ ->
					error_logger:warning_msg("UNKNOWN: ~p~n", [Path]),
					Req:respond({404, [], "404 Not found\r\n"})
			end;
		_ ->
			Headers = [{"Allow", "GET"}],
			Req:respond({405, Headers, "405 Method Not Allowed\r\n"})
	end.

drop_dupes(List) ->
	drop_dupes(lists:sort(List), []).

drop_dupes([], List) ->
	List;
drop_dupes([{A, B}, {A, B} | Rest], Ret) ->
	drop_dupes([{A, B} | Rest], Ret);
drop_dupes([{A, B} | Rest], Ret) ->
	drop_dupes(Rest, Ret ++ [{A,B}]).
