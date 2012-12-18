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

-module(http_server).
-author('lemenkov@gmail.com').

-export([dispatch/1]).

dispatch(Req) ->
	case Req:get(method) of
		'GET' ->
			% Build JSON
			Path = Req:get(path),
			case string:tokens(Path, "/") of
				["html" | _] ->
					JSON = "{hello:\"html\"}",
					Req:respond({200, [], JSON});
				["json" | _] ->
					JSON = case Req:parse_qs() of
						[] -> dump_all();
						KV -> dump_query(KV)
					end,
					Req:respond({200, [], JSON});
				_ ->
					error_logger:warning_msg("UNKNOWN: ~p~n", [Path]),
					Req:respond({404, [], "404 Not found\r\n"})
			end;
		_ ->
			Headers = [{"Allow", "GET"}],
			Req:respond({405, Headers, "405 Method Not Allowed\r\n"})
	end.

dump_all() ->
	Length = length(gproc:lookup_global_properties(media)) div 2,
	List = gproc:select([{ { {p,g, media} , '_' , {'_', '_', '_', '_', '_', '_'} }, [], ['$$']}]),
	Result =  [	{media,
					[
						{callid, CallId},
						{mediaid, MediaId},
						{tag, Tag},
						{payload, Payload},
						{local, [{ip, list_to_binary(inet_parse:ntoa(LocalIp))}, {rtp, LocalRtpPort}, {rtcp, LocalRtcpPort}]},
						{remote,[{ip, list_to_binary(inet_parse:ntoa(RemoteIp))}, {rtp, RemoteRtpPort}, {rtcp, RemoteRtcpPort}]}
					]
				} || [{p,g,media}, _, {CallId, MediaId, Tag, Payload, {LocalIp, LocalRtpPort, LocalRtcpPort}, {RemoteIp, RemoteRtpPort, RemoteRtcpPort}}] <- List],
	mochijson2:encode([{callnum, Length}, {calls, Result}]).

dump_query(RawQuery) ->
	Query = [ decode_kv(KV) || KV <- RawQuery],
	C = proplists:get_value(callid, Query, '_'),
	M = proplists:get_value(mediaid, Query, '_'),
	T = proplists:get_value(tag, Query, '_'),
	P = proplists:get_value(payload, Query, '_'),
	I = proplists:get_value(remoteip, Query, '_'),

	%% C M T Payload Local Remote
	List = gproc:select([{ { {p,g, media} , '_' , {C, M, T, P, '_', {I,'_','_'}} }, [], ['$$']}]),
	Result = [
			{media,
				[
					{callid, CallId},
					{mediaid, MediaId},
					{tag, Tag},
					{payload, Payload},
					{local, [{ip, list_to_binary(inet_parse:ntoa(LocalIp))}, {rtp, LocalRtpPort}, {rtcp, LocalRtcpPort}]},
					{remote,[{ip, list_to_binary(inet_parse:ntoa(RemoteIp))}, {rtp, RemoteRtpPort}, {rtcp, RemoteRtcpPort}]}
				]
			} || [{p,g,media}, _, {CallId, MediaId, Tag, Payload, {LocalIp, LocalRtpPort, LocalRtcpPort}, {RemoteIp, RemoteRtpPort, RemoteRtcpPort}}] <- List],
	mochijson2:encode([{http_query,  [ {list_to_existing_atom(K), list_to_binary(V)} || {K,V} <- RawQuery]}, {result, Result}]).

decode_kv({"callid", C}) ->
	{callid, list_to_binary(C)};
decode_kv({"mediaid", M}) ->
	{mediaid, list_to_binary(M)};
decode_kv({"tag", T}) ->
	{tag, list_to_binary(T)};
decode_kv({"payload", P}) ->
	{payload, list_to_integer(P)};
decode_kv({"remoteip", I}) ->
	{ok, Ip} = inet_parse:address(I),
	{remoteip, Ip}.
