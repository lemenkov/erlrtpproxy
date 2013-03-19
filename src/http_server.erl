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
					Req:respond({200, [], dump_query(Req:parse_qs())});
				["params" | _] ->
					Qs = Req:parse_qs(),
					case Qs of
						[] ->
							Req:respond({200, [], get_params()});
						_ ->
							set_params(Qs),
							Req:respond({200, [], "Done"})
					end;
				_ ->
					error_logger:warning_msg("UNKNOWN: ~p~n", [Path]),
					Req:respond({404, [], "404 Not found\r\n"})
			end;
		_ ->
			Headers = [{"Allow", "GET"}],
			Req:respond({405, Headers, "405 Method Not Allowed\r\n"})
	end.

dump_query([{"callnum",_}]) ->
	mochijson2:encode([{callnum, gproc:get_value({c, g, calls}, shared)}]);
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
			begin
			{RemoteIp, RemoteRtpPort, RemoteRtcpPort, SSRC, Payload, RxBytes, RxPackets, TxBytes, TxPackets, Rr0, Sr0} = gen_server:call(Pid, get_stats),
			Rr = case Rr0 of null -> {rr, null}; false -> {rr, null}; _ ->  rtp_utils:to_proplist(Rr0) end,
			Sr = case Sr0 of null -> {sr, null}; false -> {sr, null}; _ ->  rtp_utils:to_proplist(Sr0) end,
			[{media,
				[
					{callid, CallId},
					{mediaid, MediaId},
					{tag, Tag},
					{payload, Payload},
					{rxbytes, RxBytes},
					{rxpackets, RxPackets},
					{txbytes, TxBytes},
					{txpackets, TxPackets},
					{ssrc, SSRC},
					{local, [{ip, make_ip(LocalIp)}, {rtp, LocalRtpPort}, {rtcp, LocalRtcpPort}]},
					{remote,[{ip, make_ip(RemoteIp)}, {rtp, RemoteRtpPort}, {rtcp, RemoteRtcpPort}]},
					Rr,
					Sr
				]
			}]
			end
	|| [{p,g,media}, Pid, {CallId, MediaId, Tag, _, {LocalIp, LocalRtpPort, LocalRtcpPort}, {_, _, _}}] <- List],

	mochijson2:encode([{http_query,  Query}, {num, length(List)}, {calllegs, Result}]).

get_params() ->
	Params = [ttl, ttl_early, rebuildrtp, ignore_start, ignore_stop, sendrecv, active],
	Nodes = pool:get_nodes(),
	JSON = lists:map(fun(Node) ->
				[{node, Node}, {params, lists:map(fun(X) -> {ok, Y} = rpc:call(Node, application, get_env, [rtpproxy, X]), {X, Y} end, Params)}]
			end, Nodes),

	mochijson2:encode(JSON).

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
	{sendrecv, enforcing};
decode_kv({"active", "once"}) ->
	{active, once};
decode_kv({"active", "true"}) ->
	{active, true}.

make_ip(null) ->
	<<"null">>;
make_ip(Ip) ->
	list_to_binary(inet_parse:ntoa(Ip)).
