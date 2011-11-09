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

-define(RTPPROXY_OK, " 0\n").
-define(RTPPROXY_VER_SUPPORTED, " 1\n").
-define(RTPPROXY_ERR_SYNTAX,    " E1\n").
-define(RTPPROXY_ERR_SOFTWARE,  " E7\n").
-define(RTPPROXY_ERR_NOSESSION, " E8\n").

-define(SAFE_PARTY(Val), case Val of null -> null; _ -> #party{tag=Val} end).

decode(Msg) ->
	% Cut last \n if it is exist
	M = case lists:reverse(Msg) of
		[$\n | Msg1] -> lists:reverse(Msg1);
		_ -> Msg
	end,
	% Drop accidental zeroes - OpenSIPs inserts them sometimes
	% FIXME bug in OpenSIPS?
	[Cookie,C|Rest] = string:tokens([X || X <-  M, X /= 0], " ;"),
	case parse_splitted([string:to_upper(C)|Rest]) of
		#cmd{} = Cmd ->
			Cmd#cmd{
				cookie=Cookie,
				origin=#origin{
					type=ser,
					pid=self()
				}
			};
		#response{} = Response ->
			Response#response{
				cookie=Cookie
			}
	end.

encode({error, syntax, Msg}) when is_list(Msg) ->
	[Cookie|_Rest] = string:tokens(Msg, " "),
	Cookie ++ ?RTPPROXY_ERR_SYNTAX;
encode(#response{cookie = Cookie, type = reply, data = ok}) ->
	Cookie ++ ?RTPPROXY_OK;
encode(#response{cookie = Cookie, type = reply, data = supported}) ->
	Cookie ++ ?RTPPROXY_VER_SUPPORTED;
encode(#response{cookie = Cookie, type = reply, data = {version, Version}}) when is_list(Version) ->
	Cookie ++ " " ++ Version ++ "\n";
encode(#response{cookie = Cookie, type = reply, data = {{{I0,I1,I2,I3} = IPv4, Port}, _}}) when
	is_integer(I0), I0 >= 0, I0 < 256,
	is_integer(I1), I1 >= 0, I1 < 256,
	is_integer(I2), I2 >= 0, I2 < 256,
	is_integer(I3), I3 >= 0, I3 < 256 ->
	Cookie ++ " " ++ integer_to_list(Port) ++ " " ++ inet_parse:ntoa(IPv4) ++ "\n";
encode(#response{cookie = Cookie, type = reply, data = {{{I0,I1,I2,I3,I4,I5,I6,I7} = IPv6, Port}, _}}) when
	is_integer(I0), I0 >= 0, I0 < 65535,
	is_integer(I1), I1 >= 0, I1 < 65535,
	is_integer(I2), I2 >= 0, I2 < 65535,
	is_integer(I3), I3 >= 0, I3 < 65535,
	is_integer(I4), I4 >= 0, I4 < 65535,
	is_integer(I5), I5 >= 0, I5 < 65535,
	is_integer(I6), I6 >= 0, I6 < 65535,
	is_integer(I7), I7 >= 0, I7 < 65535 ->
	Cookie ++ " " ++ integer_to_list(Port) ++ " " ++ inet_parse:ntoa(IPv6) ++ "\n";
encode(#response{cookie = Cookie, type = error, data = syntax}) ->
	Cookie ++ ?RTPPROXY_ERR_SYNTAX;
encode(#response{cookie = Cookie, type = error, data = software}) ->
	Cookie ++ ?RTPPROXY_ERR_SOFTWARE;
encode(#response{cookie = Cookie, type = error, data = notfound}) ->
	Cookie ++ ?RTPPROXY_ERR_NOSESSION;
encode(#response{cookie = Cookie, type = Type, data = Data}) ->
	ok;

encode(#cmd{cookie = Cookie, type = ?CMD_V}) ->
	Cookie ++ " V" ++ "\n";

encode(#cmd{cookie = Cookie, type = ?CMD_VF, params = Version}) ->
	Cookie ++ " VF " ++ Version ++ "\n";

encode(#cmd{cookie = Cookie, type = ?CMD_D, callid = CallId, from = #party{tag = FromTag}, to = null}) ->
	Cookie ++ " D " ++ CallId ++ " " ++ FromTag ++ "\n";
encode(#cmd{cookie = Cookie, type = ?CMD_D, callid = CallId, from = #party{tag = FromTag}, to = #party{tag = ToTag}}) ->
	Cookie ++ " D " ++ CallId ++ " " ++ FromTag ++ " " ++ ToTag ++ "\n";

encode(#cmd{cookie = Cookie, type = ?CMD_S, callid = CallId, mediaid = MediaId, from = #party{tag = FromTag}, to = null}) ->
	[M] = io_lib:format("~w", [MediaId]),
	Cookie ++ " S " ++ CallId ++ " " ++ FromTag ++ ";" ++ M ++ "\n";
encode(#cmd{cookie = Cookie, type = ?CMD_S, callid = CallId, mediaid = MediaId, from = #party{tag = FromTag}, to = #party{tag = ToTag}}) ->
	[M] = io_lib:format("~w", [MediaId]),
	Cookie ++ " S " ++ CallId ++ " " ++ FromTag ++ ";" ++ M ++ " " ++ ToTag ++ ";" ++ M ++ "\n";

encode(#cmd{cookie = Cookie, type = ?CMD_R, callid = CallId, from = #party{tag = FromTag}, to = null}) ->
	Cookie ++ " R " ++ CallId ++ " " ++ FromTag ++ "\n";
encode(#cmd{cookie = Cookie, type = ?CMD_R, callid = CallId, from = #party{tag = FromTag}, to = #party{tag = ToTag}}) ->
	Cookie ++ " R " ++ CallId ++ " " ++ FromTag ++ " " ++ ToTag ++ "\n";

encode(#cmd{cookie = Cookie, type = ?CMD_Q, callid = CallId, mediaid = MediaId, from = #party{tag = FromTag}, to = #party{tag = ToTag}}) ->
	[M] = io_lib:format("~w", [MediaId]),
	Cookie ++ " Q " ++ CallId ++ " " ++ FromTag ++ ";" ++ M ++ " " ++ ToTag ++ ";" ++ M ++ "\n";

encode(#cmd{cookie = Cookie, type = ?CMD_X, params = Version}) ->
	Cookie ++ " X\n";

encode(#cmd{cookie = Cookie, type = ?CMD_I, params = Version}) ->
	Cookie ++ " I\n";

encode(_) ->
	throw({error_syntax, "Unknown (or unsupported) #cmd"}).

%%
%% Private functions
%%

%%
%% Requests
%%

% Request basic supported rtpproxy protocol version
parse_splitted(["V"]) ->
	#cmd{
		type=?CMD_V
	};

% Request additional rtpproxy protocol extensions
parse_splitted(["VF", "20040107"]) ->
	% Basic RTP proxy functionality
	#cmd{type=?CMD_VF, params="20040107"};
parse_splitted(["VF", "20050322"]) ->
	% Support for multiple RTP streams and MOH
	#cmd{type=?CMD_VF, params="20050322"};
parse_splitted(["VF", "20060704"]) ->
	% Support for extra parameter in the V command
	#cmd{type=?CMD_VF, params="20060704"};
parse_splitted(["VF", "20071116"]) ->
	% Support for RTP re-packetization
	#cmd{type=?CMD_VF, params="20071116"};
parse_splitted(["VF", "20071218"]) ->
	% Support for forking (copying) RTP stream
	#cmd{type=?CMD_VF, params="20071218"};
parse_splitted(["VF", "20080403"]) ->
	% Support for RTP statistics querying
	#cmd{type=?CMD_VF, params="20080403"};
parse_splitted(["VF", "20081102"]) ->
	% Support for setting codecs in the update/lookup command
	#cmd{type=?CMD_VF, params="20081102"};
parse_splitted(["VF", "20081224"]) ->
	% Support for session timeout notifications
	#cmd{type=?CMD_VF, params="20081224"};
parse_splitted(["VF", "20090810"]) ->
	% Support for automatic bridging
	#cmd{type=?CMD_VF, params="20090810"};
parse_splitted(["VF", Unknown]) ->
	throw({error_syntax, "Unknown version: " ++ Unknown});

% Create session (no ToTag)
parse_splitted([[$U|Args], CallId, ProbableIp, ProbablePort, FromTag, MediaId]) ->
	parse_splitted([[$U|Args], CallId, ProbableIp, ProbablePort, FromTag, MediaId, null, MediaId]);
% Reinvite, Hold and Resume
parse_splitted([[$U|Args], CallId, ProbableIp, ProbablePort, FromTag, MediaId, ToTag, MediaId]) ->
	{GuessIp, GuessPort} = parse_addr(ProbableIp, ProbablePort),
	Params0 = decode_params(Args),

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
		mediaid	= parse_media_id(MediaId),
		from = #party{tag=FromTag, addr=Addr, rtcpaddr=RtcpAddr, proto=proplists:get_value(proto, Params1, udp)},
		to = ?SAFE_PARTY(ToTag),
		params = lists:sort(proplists:delete(proto, Params1))
	};

% Lookup existing session
% In fact it differs from CMD_U only by the order of tags
parse_splitted([[$L|Args], CallId, ProbableIp, ProbablePort, FromTag, MediaId, ToTag, MediaId]) ->
	Cmd = parse_splitted([[$U|Args], CallId, ProbableIp, ProbablePort, ToTag, MediaId, FromTag, MediaId]),
	Cmd#cmd{type = ?CMD_L};

% delete session (no MediaIds and no ToTag) - Cancel
parse_splitted(["D", CallId, FromTag]) ->
	parse_splitted(["D", CallId, FromTag, null]);
% delete session (no MediaIds) - Bye
parse_splitted(["D", CallId, FromTag, ToTag]) ->
	#cmd{
		type=?CMD_D,
		callid=CallId,
		from=#party{tag=FromTag},
		to = ?SAFE_PARTY(ToTag)
	};

% Playback pre-recorded audio (Music-on-hold and resume, no ToTag)
parse_splitted([[$P|Args], CallId, PlayName, Codecs, FromTag, MediaId]) ->
	#cmd{
		type=?CMD_P,
		callid=CallId,
		mediaid=parse_media_id(MediaId),
		from=#party{tag=FromTag},
		params=lists:sort(parse_playcount(Args) ++ [{filename, PlayName}, {codecs, Codecs}])
	};
% Playback pre-recorded audio (Music-on-hold and resume)
parse_splitted([[$P|Args], CallId, PlayName, Codecs, FromTag, MediaId, ToTag, MediaId]) ->
	#cmd{
		type=?CMD_P,
		callid=CallId,
		mediaid=parse_media_id(MediaId),
		from=#party{tag=FromTag},
		to=#party{tag=ToTag},
		params=lists:sort(parse_playcount(Args) ++ [{filename, PlayName}, {codecs, Codecs}])
	};
% Playback pre-recorded audio (Music-on-hold and resume)
parse_splitted([[$P|Args], CallId, PlayName, Codecs, FromTag, MediaId, ToTag, MediaId, ProbableIp, ProbablePort]) ->
	{GuessIp, GuessPort} = parse_addr(ProbableIp, ProbablePort),
	#cmd{
		type=?CMD_P,
		callid=CallId,
		mediaid=parse_media_id(MediaId),
		from=#party{tag=FromTag},
		to=#party{tag=ToTag},
		params=lists:sort(parse_playcount(Args) ++ [{filename, PlayName}, {codecs, Codecs}, {addr, {GuessIp, GuessPort}}])
	};

% Stop playback or record (no ToTag)
parse_splitted(["S", CallId, FromTag, MediaId]) ->
	parse_splitted(["S", CallId, FromTag, MediaId, null, MediaId]);
% Stop playback or record
parse_splitted(["S", CallId, FromTag, MediaId, ToTag, MediaId]) ->
	#cmd{
		type=?CMD_S,
		callid=CallId,
		mediaid=parse_media_id(MediaId),
		from=#party{tag=FromTag},
		to = ?SAFE_PARTY(ToTag)
	};

% Record (obsoleted in favor of Copy)
% No MediaIds and no ToTag
parse_splitted(["R", CallId, FromTag]) ->
	Cmd = parse_splitted(["C", CallId, default, FromTag, "0", null, "0"]),
	Cmd#cmd{type = ?CMD_R};
% Record (obsoleted in favor of Copy)
% No MediaIds
parse_splitted(["R", CallId, FromTag, ToTag]) ->
	Cmd = parse_splitted(["C", CallId, default, FromTag, "0", ToTag, "0"]),
	Cmd#cmd{type = ?CMD_R};
% Copy session (same as record, which is now obsolete)
parse_splitted(["C", CallId, RecordName, FromTag, MediaId, ToTag, MediaId]) ->
	#cmd{
		type=?CMD_C,
		callid=CallId,
		mediaid=parse_media_id(MediaId),
		from=#party{tag=FromTag},
		to = ?SAFE_PARTY(ToTag),
		params=[{filename, RecordName}]
	};

% Query information about one particular session
parse_splitted(["Q", CallId, FromTag, MediaId, ToTag, MediaId]) ->
	#cmd{
		type=?CMD_Q,
		callid=CallId,
		mediaid=parse_media_id(MediaId),
		from=#party{tag=FromTag},
		to=#party{tag=ToTag}
	};

% Stop all active sessions
parse_splitted(["X"]) ->
	#cmd{
		type=?CMD_X
	};

% Get overall statistics
parse_splitted(["I"]) ->
	#cmd{
		type=?CMD_I
	};


%%
%% Replies
%%

parse_splitted(["0"]) ->
	#response{type = reply, data = ok};

parse_splitted(["1"]) ->
	% This really should be ok - that's another one shortcoming
	#response{type = reply, data = supported};

parse_splitted(["20040107"]) ->
	#response{type = reply, data = {version, "20040107"}};
parse_splitted(["20050322"]) ->
	#response{type = reply, data = {version, "20050322"}};
parse_splitted(["20060704"]) ->
	#response{type = reply, data = {version, "20060704"}};
parse_splitted(["20071116"]) ->
	#response{type = reply, data = {version, "20071116"}};
parse_splitted(["20071218"]) ->
	#response{type = reply, data = {version, "20071218"}};
parse_splitted(["20080403"]) ->
	#response{type = reply, data = {version, "20080403"}};
parse_splitted(["20081102"]) ->
	#response{type = reply, data = {version, "20081102"}};
parse_splitted(["20081224"]) ->
	#response{type = reply, data = {version, "20081224"}};
parse_splitted(["20090810"]) ->
	#response{type = reply, data = {version, "20090810"}};

parse_splitted(["E1"]) ->
	#response{type = error, data = syntax};

parse_splitted(["E7"]) ->
	#response{type = error, data = software};

parse_splitted(["E8"]) ->
	#response{type = error, data = notfound};

parse_splitted([P, I]) ->
	{Ip, Port} = parse_addr(I, P),
	#response{type = reply, data = {{Ip, Port}, {Ip, Port+1}}};

%%
%% Error / Unknown request or reply
%%

parse_splitted(_) ->
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
					throw({error_syntax, "Wrong port"})
			catch
				_:_ ->
					throw ({error_syntax, "Wrong port"})
			end;
		_ ->
			throw({error_syntax, "Wrong IP"})
	catch
		_:_ ->
			throw({error_syntax, "Wrong IP"})
	end.

parse_media_id(ProbableMediaId) when is_list(ProbableMediaId) ->
	try list_to_integer (ProbableMediaId)
	catch
		_:_ ->
			throw({error_syntax, "Wrong MediaID"})
	end.

parse_playcount(ProbablePlayCount) ->
	try [{playcount, list_to_integer (ProbablePlayCount)}]
	catch
		_:_ ->
			throw({error_syntax, "Wrong PlayCount"})
	end.


decode_params(A) ->
	decode_params(A, []).

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
			decode_params(Rest, Result);
		Ret ->
			Rest1 = string:substr(Rest, Ret + 1),
			% FIXME should we sort codecs at all or should we try to maintain their original order?
			Codecs = lists:map(fun guess_codec/1,
				lists:sort(
					lists:map(fun guess_codec_n/1,
						string:tokens(string:substr(Rest, 1, Ret), ",")
					)
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

decode_params([_|Rest], Result) ->
	% Unknown parameter - just skip it
	decode_params(Rest, Result).

ensure_alone(Proplist, Param) ->
	proplists:delete(Param, Proplist) ++ [Param].
ensure_alone(Proplist, Param, Value) ->
	proplists:delete(Param, Proplist) ++ [{Param, Value}].

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
