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

-module(ser_proto).
-author('lemenkov@gmail.com').

-export([decode/1]).
-export([encode/1]).

-include("common.hrl").

-define(SAFE_PARTY(Val0), case Val0 of null -> null; _ -> [Val, _] = ensure_mediaid(binary_split(Val0, $;)), #party{tag = Val} end).

decode(Msg) when is_binary(Msg) ->
	% Cut last \n if it is exist and
	% Drop accidental zeroes - OpenSIPs inserts them sometimes
	% FIXME bug in OpenSIPS?
	[Cookie,C|Rest] = binary_split(cut_newline(drop_zeroes(Msg)), $ ),
	case parse_splitted([binary_to_upper(C)|Rest]) of
		#cmd{} = Cmd ->
			Cmd#cmd{
				cookie=Cookie,
				origin=#origin{
					type=ser,
					pid=self()
				}
			};
		#response{} = Response ->
			case  Response#response.type of
				stats ->
					Response#response{
						cookie = Cookie,
						data = binary_to_list(Msg) % I contains it's own formatting
					};
				_ ->
					Response#response{
						cookie=Cookie
					}
			end
	end;

decode(Msg) when is_list(Msg) ->
	decode(list_to_binary(Msg)).

encode({error, syntax, Msg}) when is_binary(Msg) ->
	[Cookie|_] = binary_split(Msg, $ ),
	<<Cookie/binary, <<" E1\n">>/binary>>;
encode({error, software, Msg}) when is_binary(Msg) ->
	[Cookie|_] = binary_split(Msg, $ ),
	<<Cookie/binary, <<" E7\n">>/binary>>;
encode(#response{cookie = Cookie, type = reply, data = ok}) ->
	<<Cookie/binary, <<" 0\n">>/binary>>;
encode(#response{cookie = Cookie, type = reply, data = supported}) ->
	<<Cookie/binary, <<" 1\n">>/binary>>;
encode(#response{cookie = Cookie, type = reply, data = {version, Version}}) when is_binary(Version) ->
	<<Cookie/binary, <<" ">>/binary, Version/binary, <<"\n">>/binary>>;
encode(#response{cookie = Cookie, type = reply, data = {{{I0,I1,I2,I3} = IPv4, Port}, _}}) when
	is_integer(I0), I0 >= 0, I0 < 256,
	is_integer(I1), I1 >= 0, I1 < 256,
	is_integer(I2), I2 >= 0, I2 < 256,
	is_integer(I3), I3 >= 0, I3 < 256 ->
	I = list_to_binary(inet_parse:ntoa(IPv4)),
	P = list_to_binary(integer_to_list(Port)),
	<<Cookie/binary, <<" ">>/binary, P/binary, <<" ">>/binary, I/binary, <<"\n">>/binary>>;
encode(#response{cookie = Cookie, type = reply, data = {{{I0,I1,I2,I3,I4,I5,I6,I7} = IPv6, Port}, _}}) when
	is_integer(I0), I0 >= 0, I0 < 65535,
	is_integer(I1), I1 >= 0, I1 < 65535,
	is_integer(I2), I2 >= 0, I2 < 65535,
	is_integer(I3), I3 >= 0, I3 < 65535,
	is_integer(I4), I4 >= 0, I4 < 65535,
	is_integer(I5), I5 >= 0, I5 < 65535,
	is_integer(I6), I6 >= 0, I6 < 65535,
	is_integer(I7), I7 >= 0, I7 < 65535 ->
	I = list_to_binary(inet_parse:ntoa(IPv6)),
	P = list_to_binary(integer_to_list(Port)),
	<<Cookie/binary, <<" ">>/binary, P/binary, <<" ">>/binary, I/binary, <<"\n">>/binary>>;
encode(#response{cookie = Cookie, type = error, data = syntax}) ->
	<<Cookie/binary, <<" E1\n">>/binary>>;
encode(#response{cookie = Cookie, type = error, data = software}) ->
	<<Cookie/binary, <<" E7\n">>/binary>>;
encode(#response{cookie = Cookie, type = error, data = notfound}) ->
	<<Cookie/binary, <<" E8\n">>/binary>>;
encode(#response{} = Unknown) ->
	error_logger:error_msg("Unknown response: ~p~n", [Unknown]),
	throw({error_syntax, "Unknown (or unsupported) #response"});

encode(#cmd{cookie = Cookie, type = ?CMD_V}) ->
	<<Cookie/binary, <<" V\n">>/binary>>;

encode(#cmd{cookie = Cookie, type = ?CMD_VF, params = Version}) ->
	<<Cookie/binary, <<" VF ">>/binary, Version/binary, <<"\n">>/binary>>;

encode(#cmd{cookie = Cookie, type = ?CMD_U, callid = CallId, mediaid = MediaId, from = #party{tag = FromTag, addr = {GuessIp, GuessPort}}, to = null, params = Params}) ->
	ParamsBin = encode_params(Params),
	Ip = list_to_binary(inet_parse:ntoa(GuessIp)),
	Port = list_to_binary(io_lib:format("~b", [GuessPort])),
	<<Cookie/binary, <<" U">>/binary, ParamsBin/binary, <<" ">>/binary, CallId/binary, <<" ">>/binary, Ip/binary, <<" ">>/binary, Port/binary, <<" ">>/binary, FromTag/binary, <<";">>/binary, MediaId/binary, <<"\n">>/binary>>;
encode(#cmd{cookie = Cookie, type = ?CMD_U, callid = CallId, mediaid = MediaId, from = #party{tag = FromTag, addr = Addr}, to = #party{tag = ToTag}, params = Params}) ->
	ParamsBin = encode_params(Params),
	BinAddr = binary_print_addr(Addr),
	<<Cookie/binary, <<" U">>/binary, ParamsBin/binary, <<" ">>/binary, CallId/binary, <<" ">>/binary, BinAddr/binary, <<" ">>/binary, FromTag/binary, <<";">>/binary, MediaId/binary, <<" ">>/binary, ToTag/binary, <<";">>/binary, MediaId/binary, <<"\n">>/binary>>;

encode(#cmd{cookie = Cookie, type = ?CMD_L, callid = CallId, mediaid = MediaId, from = #party{tag = FromTag, addr = Addr}, to = #party{tag = ToTag}, params = Params}) ->
	ParamsBin = encode_params(Params),
	BinAddr = binary_print_addr(Addr),
	<<Cookie/binary, <<" L">>/binary, ParamsBin/binary, <<" ">>/binary, CallId/binary, <<" ">>/binary, BinAddr/binary, <<" ">>/binary, ToTag/binary, <<";">>/binary, MediaId/binary, <<" ">>/binary, FromTag/binary, <<";">>/binary, MediaId/binary, <<"\n">>/binary>>;

encode(#cmd{cookie = Cookie, type = ?CMD_D, callid = CallId, from = #party{tag = FromTag}, to = null}) ->
	<<Cookie/binary, <<" D ">>/binary, CallId/binary, <<" ">>/binary, FromTag/binary, <<"\n">>/binary>>;
encode(#cmd{cookie = Cookie, type = ?CMD_D, callid = CallId, from = #party{tag = FromTag}, to = #party{tag = ToTag}}) ->
	<<Cookie/binary, <<" D ">>/binary, CallId/binary, <<" ">>/binary, FromTag/binary, <<" ">>/binary, ToTag/binary, <<"\n">>/binary>>;

encode(
	#cmd{
		cookie = Cookie,
		type = ?CMD_P,
		callid = CallId,
		mediaid = MediaId,
		from = #party{tag = FromTag},
		to = null,
		params = [
				{codecs, Codecs},
				{filename, Filename},
				{playcount, Playcount}
			]
		}) ->
	P = list_to_binary(io_lib:format("~b", [Playcount])),
	<<Cookie/binary, <<" P">>/binary, P/binary, <<" ">>/binary, CallId/binary, <<" ">>/binary, Filename/binary, <<" ">>/binary, Codecs/binary, <<" ">>/binary, FromTag/binary, <<";">>/binary, MediaId/binary, <<"\n">>/binary>>;
encode(
	#cmd{
		cookie = Cookie,
		type = ?CMD_P,
		callid = CallId,
		mediaid = MediaId,
		from = #party{tag = FromTag},
		to = #party{tag = ToTag},
		params = [
				{codecs, Codecs},
				{filename, Filename},
				{playcount, Playcount}
			]
		}) ->
	P = list_to_binary(io_lib:format("~b", [Playcount])),
	<<Cookie/binary, <<" P">>/binary, P/binary, <<" ">>/binary, CallId/binary, <<" ">>/binary, Filename/binary, <<" ">>/binary, Codecs/binary, <<" ">>/binary, FromTag/binary, <<";">>/binary, MediaId/binary, <<" ">>/binary, ToTag/binary, <<";">>/binary, MediaId/binary, <<"\n">>/binary>>;

encode(#cmd{cookie = Cookie, type = ?CMD_S, callid = CallId, mediaid = MediaId, from = #party{tag = FromTag}, to = null}) ->
	<<Cookie/binary, <<" S ">>/binary, CallId/binary, <<" ">>/binary, FromTag/binary, <<";">>/binary, MediaId/binary, <<"\n">>/binary>>;
encode(#cmd{cookie = Cookie, type = ?CMD_S, callid = CallId, mediaid = MediaId, from = #party{tag = FromTag}, to = #party{tag = ToTag}}) ->
	<<Cookie/binary, <<" S ">>/binary, CallId/binary, <<" ">>/binary, FromTag/binary, <<";">>/binary, MediaId/binary, <<" ">>/binary, ToTag/binary, <<";">>/binary, MediaId/binary, <<"\n">>/binary>>;

encode(#cmd{cookie = Cookie, type = ?CMD_R, callid = CallId, from = #party{tag = FromTag}, to = null}) ->
	<<Cookie/binary, <<" R ">>/binary, CallId/binary, <<" ">>/binary, FromTag/binary, <<"\n">>/binary>>;
encode(#cmd{cookie = Cookie, type = ?CMD_R, callid = CallId, from = #party{tag = FromTag}, to = #party{tag = ToTag}}) ->
	<<Cookie/binary, <<" R ">>/binary, CallId/binary, <<" ">>/binary, FromTag/binary, <<" ">>/binary, ToTag/binary, <<"\n">>/binary>>;

encode(#cmd{cookie = Cookie, type = ?CMD_Q, callid = CallId, mediaid = MediaId, from = #party{tag = FromTag}, to = #party{tag = ToTag}}) ->
	<<Cookie/binary, <<" Q ">>/binary, CallId/binary, <<" ">>/binary, FromTag/binary, <<";">>/binary, MediaId/binary, <<" ">>/binary, ToTag/binary, <<";">>/binary, MediaId/binary, <<"\n">>/binary>>;

encode(#cmd{cookie = Cookie, type = ?CMD_X}) ->
	<<Cookie/binary, <<" X\n">>/binary>>;

encode(#cmd{cookie = Cookie, type = ?CMD_I, params = []}) ->
	<<Cookie/binary, <<" I\n">>/binary>>;
encode(#cmd{cookie = Cookie, type = ?CMD_I, params = [brief]}) ->
	<<Cookie/binary, <<" IB\n">>/binary>>;

encode(#cmd{} = Unknown) ->
	error_logger:error_msg("Unknown command: ~p~n", [Unknown]),
	throw({error_syntax, "Unknown (or unsupported) #cmd"}).

%%
%% Private functions
%%

%%
%% Requests
%%

% Request basic supported rtpproxy protocol version
parse_splitted([<<"V">>]) ->
	#cmd{
		type=?CMD_V
	};

% Request additional rtpproxy protocol extensions
parse_splitted([<<"VF">>, Version]) when
	Version == <<"20040107">>; % Basic RTP proxy functionality
	Version == <<"20050322">>; % Support for multiple RTP streams and MOH
	Version == <<"20060704">>; % Support for extra parameter in the V command
	Version == <<"20071116">>; % Support for RTP re-packetization
	Version == <<"20071218">>; % Support for forking (copying) RTP stream
	Version == <<"20080403">>; % Support for RTP statistics querying
	Version == <<"20081102">>; % Support for setting codecs in the update/lookup command
	Version == <<"20081224">>; % Support for session timeout notifications
	Version == <<"20090810">> -> % Support for automatic bridging
	#cmd{type=?CMD_VF, params = Version};
parse_splitted([<<"VF">>, Unknown]) ->
	throw({error_syntax, "Unknown version: " ++ binary_to_list(Unknown)});

% Create session (no ToTag, no Notify extension)
parse_splitted([<<$U:8,Args/binary>>, CallId, ProbableIp, ProbablePort, FromTag]) ->
	parse_splitted([<<$U:8,Args/binary>>, CallId, ProbableIp, ProbablePort, FromTag, null, null, null]);
% Reinvite, Hold and Resume (no Notify extension)
parse_splitted([<<$U:8,Args/binary>>, CallId, ProbableIp, ProbablePort, FromTag, ToTag]) ->
	parse_splitted([<<$U:8,Args/binary>>, CallId, ProbableIp, ProbablePort, FromTag, ToTag, null, null]);
parse_splitted([<<$U:8,Args/binary>>, CallId, ProbableIp, ProbablePort, FromTag0, ToTag, NotifyAddr, NotifyTag]) ->
	[FromTag, MediaId] = ensure_mediaid(binary_split(FromTag0, $;)),
	{GuessIp, GuessPort} = parse_addr(binary_to_list(ProbableIp), binary_to_list(ProbablePort)),
	Params0 = case {NotifyAddr, NotifyTag} of
		{null, null} -> decode_params(Args);
		_ -> decode_params(Args) ++ [{notify, [{addr, parse_notify_addr(NotifyAddr)}, {tag, NotifyTag}]}]
	end,

	% Discard address if it's not consistent with direction
	Addr = case {proplists:get_value(direction, Params0), ser_utils:is_rfc1918(GuessIp)} of
		{{external, _}, true} -> null;
		{{internal, _}, true} -> {GuessIp, GuessPort};
		{{internal, _}, false} -> null;
		{{external, _}, false} -> {GuessIp, GuessPort};
		{_, ipv6} -> {GuessIp, GuessPort}
	end,

	Params1 = case ser_utils:is_rfc1918(GuessIp) of
		ipv6 -> ensure_alone(Params0, ipv6);
		_ -> Params0
	end,

	% Try to guess RTCP address
	RtcpAddr = case Addr of
		null -> null;
		{GuessIp, GuessPort} -> {GuessIp, GuessPort + 1}
	end,

	#cmd{
		type = ?CMD_U,
		callid = CallId,
		mediaid	= MediaId,
		from = #party{tag=FromTag, addr=Addr, rtcpaddr=RtcpAddr, proto=proplists:get_value(proto, Params1, udp)},
		to = ?SAFE_PARTY(ToTag),
		params = lists:sort(proplists:delete(proto, Params1))
	};

% Lookup existing session
% In fact it differs from CMD_U only by the order of tags
parse_splitted([<<$L:8,Args/binary>>, CallId, ProbableIp, ProbablePort, FromTag, ToTag]) ->
	Cmd = parse_splitted([<<$U:8,Args/binary>>, CallId, ProbableIp, ProbablePort, ToTag, FromTag]),
	Cmd#cmd{type = ?CMD_L};

% delete session (no MediaIds and no ToTag) - Cancel
parse_splitted([<<"D">>, CallId, FromTag]) ->
	parse_splitted([<<"D">>, CallId, FromTag, null]);
% delete session (no MediaIds) - Bye
parse_splitted([<<"D">>, CallId, FromTag, ToTag]) ->
	#cmd{
		type=?CMD_D,
		callid=CallId,
		from=#party{tag=FromTag},
		to = case ToTag of null -> null; _ -> #party{tag=ToTag} end
	};

% Playback pre-recorded audio (Music-on-hold and resume, no ToTag)
parse_splitted([<<$P:8,Args/binary>>, CallId, PlayName, Codecs, FromTag0]) ->
	[FromTag, MediaId] = ensure_mediaid(binary_split(FromTag0, $;)),
	#cmd{
		type=?CMD_P,
		callid=CallId,
		mediaid=MediaId,
		from=#party{tag=FromTag},
		params=lists:sort(parse_playcount(Args) ++ [{filename, PlayName}, {codecs, Codecs}])
	};
% Playback pre-recorded audio (Music-on-hold and resume)
parse_splitted([<<$P:8,Args/binary>>, CallId, PlayName, Codecs, FromTag0, ToTag]) ->
	[FromTag, MediaId] = ensure_mediaid(binary_split(FromTag0, $;)),
	#cmd{
		type=?CMD_P,
		callid=CallId,
		mediaid=MediaId,
		from=#party{tag=FromTag},
		to = ?SAFE_PARTY(ToTag),
		params=lists:sort(parse_playcount(Args) ++ [{filename, PlayName}, {codecs, Codecs}])
	};
% Playback pre-recorded audio (Music-on-hold and resume)
parse_splitted([<<$P:8,Args/binary>>, CallId, PlayName, Codecs, FromTag0, ToTag, ProbableIp, ProbablePort]) ->
	[FromTag, MediaId] = ensure_mediaid(binary_split(FromTag0, $;)),
	{GuessIp, GuessPort} = parse_addr(binary_to_list(ProbableIp), binary_to_list(ProbablePort)),
	#cmd{
		type=?CMD_P,
		callid=CallId,
		mediaid=MediaId,
		from=#party{tag=FromTag},
		to = ?SAFE_PARTY(ToTag),
		params=lists:sort(parse_playcount(Args) ++ [{filename, PlayName}, {codecs, Codecs}, {addr, {GuessIp, GuessPort}}])
	};

% Stop playback or record (no ToTag)
parse_splitted([<<"S">>, CallId, FromTag]) ->
	parse_splitted([<<"S">>, CallId, FromTag, null]);
% Stop playback or record
parse_splitted([<<"S">>, CallId, FromTag0, ToTag]) ->
	[FromTag, MediaId] = ensure_mediaid(binary_split(FromTag0, $;)),
	#cmd{
		type=?CMD_S,
		callid=CallId,
		mediaid=MediaId,
		from=#party{tag=FromTag},
		to = ?SAFE_PARTY(ToTag)
	};

% Record (obsoleted in favor of Copy)
% No MediaIds and no ToTag
parse_splitted([<<"R">>, CallId, FromTag]) ->
	Cmd = parse_splitted([<<"C">>, CallId, default, <<FromTag/binary, <<";0">>/binary>>, null]),
	Cmd#cmd{type = ?CMD_R};
% Record (obsoleted in favor of Copy)
% No MediaIds
parse_splitted([<<"R">>, CallId, FromTag, ToTag]) ->
	Cmd = parse_splitted([<<"C">>, CallId, default, <<FromTag/binary, <<";0">>/binary>>, <<ToTag/binary, <<";0">>/binary>>]),
	Cmd#cmd{type = ?CMD_R};
% Copy session (same as record, which is now obsolete)
parse_splitted([<<"C">>, CallId, RecordName, FromTag0, ToTag]) ->
	[FromTag, MediaId] = ensure_mediaid(binary_split(FromTag0, $;)),
	#cmd{
		type=?CMD_C,
		callid=CallId,
		mediaid=MediaId,
		from=#party{tag=FromTag},
		to = ?SAFE_PARTY(ToTag),
		params=[{filename, RecordName}]
	};

% Query information about one particular session
parse_splitted([<<"Q">>, CallId, FromTag0, ToTag0]) ->
	[FromTag, MediaId] = ensure_mediaid(binary_split(FromTag0, $;)),
	[ToTag, _] = binary_split(ToTag0, $;),
	#cmd{
		type=?CMD_Q,
		callid=CallId,
		mediaid=MediaId,
		from=#party{tag=FromTag},
		to=#party{tag=ToTag}
	};

% Stop all active sessions
parse_splitted([<<"X">>]) ->
	#cmd{
		type=?CMD_X
	};

% Get overall statistics
parse_splitted([<<"I">>]) ->
	#cmd{
		type=?CMD_I,
		params=[]
	};
parse_splitted([<<"IB">>]) ->
	#cmd{
		type=?CMD_I,
		params=[brief]
	};

%%
%% Replies
%%

parse_splitted([<<"0">>]) ->
	#response{type = reply, data = ok};

parse_splitted([<<"1">>]) ->
	% This really should be ok - that's another one shortcoming
	#response{type = reply, data = supported};

parse_splitted([<<"20040107">>]) ->
	#response{type = reply, data = {version, <<"20040107">>}};
parse_splitted([<<"20050322">>]) ->
	#response{type = reply, data = {version, <<"20050322">>}};
parse_splitted([<<"20060704">>]) ->
	#response{type = reply, data = {version, <<"20060704">>}};
parse_splitted([<<"20071116">>]) ->
	#response{type = reply, data = {version, <<"20071116">>}};
parse_splitted([<<"20071218">>]) ->
	#response{type = reply, data = {version, <<"20071218">>}};
parse_splitted([<<"20080403">>]) ->
	#response{type = reply, data = {version, <<"20080403">>}};
parse_splitted([<<"20081102">>]) ->
	#response{type = reply, data = {version, <<"20081102">>}};
parse_splitted([<<"20081224">>]) ->
	#response{type = reply, data = {version, <<"20081224">>}};
parse_splitted([<<"20090810">>]) ->
	#response{type = reply, data = {version, <<"20090810">>}};

parse_splitted([<<"E1">>]) ->
	#response{type = error, data = syntax};

parse_splitted([<<"E7">>]) ->
	#response{type = error, data = software};

parse_splitted([<<"E8">>]) ->
	#response{type = error, data = notfound};

parse_splitted([P, I]) ->
	{Ip, Port} = parse_addr(binary_to_list(I), binary_to_list(P)),
	#response{type = reply, data = {{Ip, Port}, {Ip, Port+1}}};

% FIXME Special case - stats
parse_splitted(["SESSIONS", "created:" | Rest]) ->
	#response{type = stats};

%%
%% Error / Unknown request or reply
%%

parse_splitted(Unknown) ->
	error_logger:error_msg("Unknown command: ~p~n", [Unknown]),
	throw({error_syntax, "Unknown command"}).

%%
%% Internal functions
%%

parse_addr(ProbableIp, ProbablePort) ->
	try inet_parse:address(ProbableIp) of
		{ok, GuessIp} ->
			try list_to_integer(ProbablePort) of
				GuessPort when GuessPort >= 0, GuessPort < 65536 ->
					{GuessIp, GuessPort};
				_ ->
					throw({error_syntax, {"Wrong port", ProbablePort}})
			catch
				_:_ ->
					throw({error_syntax, {"Wrong port", ProbablePort}})
			end;
		_ ->
			throw({error_syntax, {"Wrong IP", ProbableIp}})
	catch
		_:_ ->
			throw({error_syntax, {"Wrong IP", ProbableIp}})
	end.

parse_playcount(ProbablePlayCount) ->
	error_logger:error_msg("ProbablePlayCount: ~p~n", [ProbablePlayCount]),
	try [{playcount, list_to_integer (binary_to_list(ProbablePlayCount))}]
	catch
		_:_ ->
			throw({error_syntax, {"Wrong PlayCount", ProbablePlayCount}})
	end.

parse_notify_addr(NotifyAddr) ->
	case binary_split(NotifyAddr, $:) of
		[Port] ->
			list_to_integer(binary_to_list(Port));
		[IP, Port] ->
			parse_addr(binary_to_list(IP), binary_to_list(Port));
		List when is_list(List) -> % IPv6 probably FIXME
			throw({error, ipv6notsupported})
	end.

decode_params(A) ->
	decode_params(binary_to_list(A), []).

decode_params([], Result) ->
	% Default parameters are - symmetric NAT, non-RFC1918 IPv4 network
	R1 = case proplists:get_value(direction, Result) of
		undefined ->
			Result ++ [{direction, {external, external}}];
		_ ->
			Result
	end,
	R2 = case {proplists:get_value(asymmetric, R1), proplists:get_value(symmetric, R1)} of
		{true, true} ->
			throw({error_syntax, "Both symmetric and asymmetric modifiers are defined"});
		{true, _} ->
			proplists:delete(asymmetric, R1) ++ [{symmetric, false}];
		_ ->
			proplists:delete(symmetric, R1) ++ [{symmetric, true}]
	end,
	R3 = case {proplists:get_value(transcode, R2), proplists:get_value(codecs, R2)} of
		{undefined, _} ->
			R2;
		{_, undefined} ->
			% Requested transcoding but no codecs are available - kill transcode
			proplists:delete(transcode, R2);
		{Codec, Codecs} ->
			case lists:member(Codec, Codecs) of
				true ->
					R2;
				_ ->
					% Requested transcoding to incompatible codec - kill transcode
					proplists:delete(transcode, R2)
			end
	end,
	lists:sort(R3);
% IPv6
decode_params([$6|Rest], Result) ->
	decode_params(Rest, ensure_alone(Result, ipv6));
% Asymmetric
decode_params([$A|Rest], Result) ->
	decode_params(Rest, ensure_alone(proplists:delete(symmetric, Result), asymmetric));
% c0,101,100 - Codecs (a bit tricky)
decode_params([$C|Rest], Result) ->
	case string:span(Rest, "0123456789,") of
		0 ->
			% Bogus - skip incomplete modifier
			error_logger:error_msg("Found C parameter w/o necessary values - skipping~n"),
			decode_params(Rest, Result);
		Ret ->
			Rest1 = string:substr(Rest, Ret + 1),
			Codecs = lists:map(fun guess_codec/1,
				lists:map(fun guess_codec_n/1,
					string:tokens(string:substr(Rest, 1, Ret), ",")
				)
			),
			decode_params(Rest1, ensure_alone(Result, codecs, Codecs))
	end;
% Direction:
% External (non-RFC1918) network
% Internal (RFC1918) network
% External to External
decode_params([$E, $E|Rest], Result) ->
	decode_params(Rest, ensure_alone(Result, direction, {external, external}));
% External to Internal
decode_params([$E, $I|Rest], Result) ->
	decode_params(Rest, ensure_alone(Result, direction, {external, internal}));
% External to External (single E)
decode_params([$E|Rest], Result) ->
	decode_params(Rest, ensure_alone(Result, direction, {external, external}));
% Internal to External
decode_params([$I, $E|Rest], Result) ->
	decode_params(Rest, ensure_alone(Result, direction, {internal, external}));
% Internal to Internal
decode_params([$I, $I|Rest], Result) ->
	decode_params(Rest, ensure_alone(Result, direction, {internal, internal}));
% Internal to Internal (single I)
decode_params([$I|Rest], Result) ->
	decode_params(Rest, ensure_alone(Result, direction, {internal, internal}));
% l - local address (?)
decode_params([$L|Rest], Result) ->
	decode_params(Rest, ensure_alone(Result, local));
% r - remote address (?)
decode_params([$R|Rest], Result) ->
	decode_params(Rest, ensure_alone(Result, remote));
% Symmetric
decode_params([$S|Rest], Result) ->
	decode_params(Rest, ensure_alone(Result, symmetric));
% Weak
decode_params([$W|Rest], Result) ->
	decode_params(Rest, ensure_alone(Result, weak));
% zNN - repacketization, NN in msec, for the most codecs its value should be
%       in 10ms increments, however for some codecs the increment could differ
%       (e.g. 30ms for GSM or 20ms for G.723).
decode_params([$Z|Rest], Result) ->
	case string:span(Rest, "0123456789") of
		0 ->
			% Bogus - skip incomplete modifier
			error_logger:error_msg("Found Z parameter w/o necessary values - skipping~n"),
			decode_params(Rest, Result);
		Ret ->
			Rest1 = string:substr(Rest, Ret + 1),
			{Value, _} = string:to_integer(string:substr(Rest, 1, Ret)),
			decode_params(Rest1, ensure_alone(Result, repacketize, Value))
	end;

%% Extensions

% Protocol - unofficial extension
decode_params([$P, $0 |Rest], Result) ->
	decode_params(Rest, ensure_alone(Result, proto, udp));
decode_params([$P, $1 |Rest], Result) ->
	decode_params(Rest, ensure_alone(Result, proto, tcp));
decode_params([$P, $2 |Rest], Result) ->
	decode_params(Rest, ensure_alone(Result, proto, sctp));
% Transcode - unofficial extension
decode_params([$T|Rest], Result) ->
	case string:span(Rest, "0123456789") of
		0 ->
			% Bogus - skip incomplete modifier
			error_logger:error_msg("Found T parameter w/o necessary values - skipping~n"),
			decode_params(Rest, Result);
		Ret ->
			Rest1 = string:substr(Rest, Ret + 1),
			{Value, _} = string:to_integer(string:substr(Rest, 1, Ret)),
			decode_params(Rest1, ensure_alone(Result, transcode, guess_codec(Value)))
	end;
% Accounting - unofficial extension
decode_params([$V, $0 |Rest], Result) ->
	decode_params(Rest, ensure_alone(Result, acc, start));
decode_params([$V, $1 |Rest], Result) ->
	decode_params(Rest, ensure_alone(Result, acc, interim_update));
decode_params([$V, $2 |Rest], Result) ->
	decode_params(Rest, ensure_alone(Result, acc, stop));

% Unknown parameter - just skip it
decode_params([Unknown|Rest], Result) ->
	error_logger:error_msg("Unsupported parameter while encoding: [~p]~n", [Unknown]),
	decode_params(Rest, Result).

encode_params(Params) ->
	encode_params(Params, []).

encode_params([], Result) ->
	list_to_binary(Result);
encode_params([ipv6|Rest], Result) ->
	encode_params(Rest, Result ++ [$6]);
encode_params([{direction, {external, external}}|Rest], Result) ->
	% FIXME
%	encode_params(Rest, Result ++ "ee");
	encode_params(Rest, Result);
encode_params([{direction, {external, internal}}|Rest], Result) ->
	encode_params(Rest, Result ++ "ei");
encode_params([{direction, {internal, external}}|Rest], Result) ->
	encode_params(Rest, Result ++ "ie");
encode_params([{direction, {internal, internal}}|Rest], Result) ->
	encode_params(Rest, Result ++ "ii");
encode_params([local|Rest], Result) ->
	encode_params(Rest, Result ++ [$l]);
encode_params([remote|Rest], Result) ->
	encode_params(Rest, Result ++ [$r]);
encode_params([{symmetric, true}|Rest], Result) ->
	% FIXME
%	encode_params(Rest, Result ++ [$s]);
	encode_params(Rest, Result);
encode_params([{symmetric, false}|Rest], Result) ->
	encode_params(Rest, Result ++ [$a]);
encode_params([weak|Rest], Result) ->
	encode_params(Rest, Result ++ [$w]);
encode_params([{codecs, Codecs}|[]], Result) ->
	% Codecs must be placed at the end of the parameters' list
	encode_params([], Result ++ [$c] ++ print_codecs(Codecs));
encode_params([{codecs, Codecs}|Rest], Result) ->
	encode_params(Rest ++ [{codecs, Codecs}], Result);
encode_params([Unknown|Rest], Result) ->
	error_logger:error_msg("Unsupported parameter while encoding: [~p]~n", [Unknown]),
	encode_params(Rest, Result).

print_codecs(Codecs) ->
	print_codecs(Codecs, []).
print_codecs([], Result) ->
	Result;
print_codecs([Codec|[]], Result) ->
	print_codecs([], Result ++ print_codec(Codec));
print_codecs([Codec|Rest], Result) ->
		print_codecs(Rest, Result ++ print_codec(Codec) ++ ",").

ensure_alone(Proplist, Param) ->
	proplists:delete(Param, Proplist) ++ [Param].
ensure_alone(Proplist, Param, Value) ->
	proplists:delete(Param, Proplist) ++ [{Param, Value}].

ensure_mediaid([Tag, MediaId]) -> [Tag, MediaId];
ensure_mediaid([Tag]) -> [Tag, <<"0">>].

% FIXME use more atoms instead of numbers where possible
% grep "a=rtpmap:" /var/log/messages | sed -e 's,.*a=rtpmap:,,g' | sort | uniq | sort -n
% http://www.iana.org/assignments/rtp-parameters
% http://www.iana.org/assignments/media-types/audio/index.html
guess_codec(0) -> {'PCMU',8000,1};
% 1 and 2 are reserved
guess_codec(3) -> {'GSM',8000,1};
guess_codec(4) -> {'G723',8000,1};
guess_codec(5) -> {'DVI4',8000,1};
guess_codec(6) -> {'DVI4',16000,1};
guess_codec(7) -> {'LPC',8000,1};
guess_codec(8) -> {'PCMA',8000,1};
guess_codec(9) -> {'G722',8000,1};
guess_codec(10) -> {'L16',8000,2}; % FIXME 44100 according to RFC3551
guess_codec(11) -> {'L16',8000,1}; % FIXME 44100 according to RFC3551
guess_codec(12) -> {'QCELP',8000,1};
guess_codec(13) -> {'CN',8000,1};
guess_codec(14) -> {'MPA',90000,0}; % FIXME 90000 Hz?
guess_codec(15) -> {'G728',8000,1};
guess_codec(16) -> {'DVI4',11025,1};
guess_codec(17) -> {'DVI4',22050,1};
guess_codec(18) -> {'G729',8000,1}; % FIXME the same as G.729a?

guess_codec(31) -> {'H261',90000,0};
guess_codec(34) -> {'H263',90000,0};

guess_codec(C) -> C.

guess_codec_n(Codec) ->
	{Y, _} = string:to_integer(Codec),
	Y.

guess_payload({'PCMU',8000,1}) -> 0;
guess_payload({'GSM',8000,1}) -> 3;
guess_payload({'G723',8000,1}) -> 4;
guess_payload({'DVI4',8000,1}) -> 5;
guess_payload({'DVI4',16000,1}) -> 6;
guess_payload({'LPC',8000,1}) -> 7;
guess_payload({'PCMA',8000,1}) -> 8;
guess_payload({'G722',8000,1}) -> 9;
guess_payload({'L16',8000,2}) -> 10;
guess_payload({'L16',8000,1}) -> 11;
guess_payload({'QCELP',8000,1}) -> 12;
guess_payload({'CN',8000,1}) -> 13;
guess_payload({'MPA',90000,0}) -> 14;
guess_payload({'G728',8000,1}) -> 15;
guess_payload({'DVI4',11025,1}) -> 16;
guess_payload({'DVI4',22050,1}) -> 17;
guess_payload({'G729',8000,1}) -> 18;
guess_payload({'H261',90000,0}) -> 31;
guess_payload({'H263',90000,0}) -> 34;
guess_payload(Number) when is_integer(Number) -> Number.

print_codec(Codec) ->
	Num = guess_payload(Codec),
	[Str] = io_lib:format("~b", [Num]),
	Str.

%%
%% Binary helper functions
%%

drop_zeroes(Binary) when is_binary(Binary) ->
	drop_zeroes(<<>>, Binary).
drop_zeroes(Result, <<>>) ->
	Result;
drop_zeroes(Result, <<0:8, Rest/binary>>) ->
	drop_zeroes(Result, Rest);
drop_zeroes(Result, <<Symbol:8, Rest/binary>>) ->
	drop_zeroes(<<Result/binary, Symbol:8>>, Rest).

cut_newline(Binary) when is_binary(Binary) ->
	Size = size(Binary) - 1,
	case Binary of
		<<Ret:Size/binary, $\n:8>> -> Ret;
		_ -> Binary
	end.

binary_to_upper(Binary) when is_binary(Binary) ->
	binary_to_upper(<<>>, Binary).
binary_to_upper(Result, <<>>) ->
	Result;
binary_to_upper(Result, <<C:8, Rest/binary>>) when $a =< C, C =< $z ->
	Symbol = C - 32,
	binary_to_upper(<<Result/binary, Symbol:8>>, Rest);
binary_to_upper(Result, <<C:8, Rest/binary>>) ->
	binary_to_upper(<<Result/binary, C:8>>, Rest).

binary_split(Binary, Val) when is_binary(Binary) ->
	binary_split(<<>>, Binary, Val, []).

binary_split(Head, <<>>, _Val, Result) ->
	lists:reverse([Head | Result]);
binary_split(Head, <<Val:8, Rest/binary>>, Val, Result) ->
	binary_split(<<>>, Rest, Val, [Head | Result]);
binary_split(Head, <<OtherVal:8, Rest/binary>>, Val, Result) ->
	binary_split(<<Head/binary, OtherVal:8>>, Rest, Val, Result).

binary_print_addr({Ip, Port}) ->
	BinIp = list_to_binary(inet_parse:ntoa(Ip)),
	BinPort = list_to_binary(io_lib:format("~b", [Port])),
	<<BinIp/binary, <<" ">>/binary, BinPort/binary>>;
binary_print_addr(null) ->
	<<"127.0.0.1 10000">>.
