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
				["params" | _] ->
					set_params(Req:parse_qs()),
					Req:respond({200, [], "Done"});
				_ ->
					error_logger:warning_msg("UNKNOWN: ~p~n", [Path]),
					Req:respond({404, [], "404 Not found\r\n"})
			end;
		_ ->
			Headers = [{"Allow", "GET"}],
			Req:respond({405, Headers, "405 Method Not Allowed\r\n"})
	end.

dump_all() ->
	Length = gproc:get_value({c, g, calls}, shared),
	List = gproc:select([{ { {p,g, media} , '_' , {'_', '_', '_', '_', '_', '_'} }, [], ['$$']}]),
	Result =  [	{media,
					[
						{callid, CallId},
						{mediaid, MediaId},
						{tag, Tag},
						{payload, Payload},
						{rxbytes, gproc:get_value({c, g, {CallId, MediaId, Tag, rxbytes}}, Pid)},
						{rxpackets, gproc:get_value({c, g, {CallId, MediaId, Tag, rxpackets}}, Pid)},
						{txbytes, gproc:get_value({c, g, {CallId, MediaId, Tag, txbytes}}, Pid)},
						{txpackets, gproc:get_value({c, g, {CallId, MediaId, Tag, txpackets}}, Pid)},
						{local, [{ip, list_to_binary(inet_parse:ntoa(LocalIp))}, {rtp, LocalRtpPort}, {rtcp, LocalRtcpPort}]},
						{remote,[{ip, list_to_binary(inet_parse:ntoa(RemoteIp))}, {rtp, RemoteRtpPort}, {rtcp, RemoteRtcpPort}]}
					]
				} || [{p,g,media}, Pid, {CallId, MediaId, Tag, Payload, {LocalIp, LocalRtpPort, LocalRtcpPort}, {RemoteIp, RemoteRtpPort, RemoteRtcpPort}}] <- List],
	mochijson2:encode([{callnum, Length}, {calls, Result}]).

dump_query([{"callnum",_}]) ->
	Length = gproc:get_value({c, g, calls}, shared),
	mochijson2:encode([{callnum, Length}]);
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
					{rxbytes, gproc:get_value({c, g, {CallId, MediaId, Tag, rxbytes}}, Pid)},
					{rxpackets, gproc:get_value({c, g, {CallId, MediaId, Tag, rxpackets}}, Pid)},
					{txbytes, gproc:get_value({c, g, {CallId, MediaId, Tag, txbytes}}, Pid)},
					{txpackets, gproc:get_value({c, g, {CallId, MediaId, Tag, txpackets}}, Pid)},
					{local, [{ip, list_to_binary(inet_parse:ntoa(LocalIp))}, {rtp, LocalRtpPort}, {rtcp, LocalRtcpPort}]},
					{remote,[{ip, list_to_binary(inet_parse:ntoa(RemoteIp))}, {rtp, RemoteRtpPort}, {rtcp, RemoteRtcpPort}]}
				]
			} || [{p,g,media}, Pid, {CallId, MediaId, Tag, Payload, {LocalIp, LocalRtpPort, LocalRtcpPort}, {RemoteIp, RemoteRtpPort, RemoteRtcpPort}}] <- List],
	mochijson2:encode([{http_query,  [ {list_to_existing_atom(K), list_to_binary(V)} || {K,V} <- RawQuery]}, {result, Result}]).

set_params(RawQuery) ->
	Query = [ decode_kv(KV) || KV <- RawQuery],
	[ rpc:multicall(pool:get_nodes(), application, set_env, [rtpproxy, K, V]) || {K,V} <- Query ],
	case proplists:get_value(save, Query) of
		true ->
			%% Save current config
			{ok,[[ConfigPath]]} = init:get_argument(config),
			error_logger:error_msg("SAVE: [~s]~n", [ConfigPath]),
			%% Run on all connected nodes
			rpc:multicall(pool:get_nodes(), rtpproxy_ctl, save_config, [ConfigPath]),
			ok;
		_ -> ok
	end.

%% For query
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
	{remoteip, Ip};

%% For parameter
decode_kv({"save",_}) ->
	save;
%decode_kv({"internal", I}) ->
%	{ok, Ip} = inet_parse:address(I),
%	{internal, Ip};
%decode_kv({"external", I}) ->
%	{ok, Ip} = inet_parse:address(I),
%	{external, Ip};
%decode_kv({"ipv6", I}) ->
%	{ok, Ip} = inet_parse:address(I),
%	{ipv6, Ip}.
decode_kv({"ttl", T}) ->
	{ttl, list_to_integer(T)};
decode_kv({"ttl_early", T}) ->
	{ttl_early, list_to_integer(T)};
decode_kv({"rebuildrtp", "true"}) ->
	{rebuildrtp, true};
decode_kv({"rebuildrtp", "false"}) ->
	{rebuildrtp, false};
decode_kv({"ignore_start", "true"}) ->
	{ignore_start, true};
decode_kv({"ignore_start", "false"}) ->
	{ignore_start, false};
decode_kv({"ignore_stop", "true"}) ->
	{ignore_stop, true};
decode_kv({"ignore_stop", "false"}) ->
	{ignore_stop, false};
decode_kv({"sendrecv", "weak"}) ->
	{sendrecv, weak};
decode_kv({"sendrecv", "roaming"}) ->
	{sendrecv, roaming};
decode_kv({"sendrecv", "enforcing"}) ->
	{sendrecv, enforcing}.
