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

-export([parse/3]).
-export([encode/2]).
-export([is_rfc1918/1]).

-include("common.hrl").

-define(RTPPROXY_OK, " 0\n").
-define(RTPPROXY_VER_SUPPORTED, " 1\n").
-define(RTPPROXY_ERR_SYNTAX,    " E1\n").
-define(RTPPROXY_ERR_SOFTWARE,  " E7\n").
-define(RTPPROXY_ERR_NOSESSION, " E8\n").

-define(SAFE_PARTY(Val), case Val of null -> null; _ -> #party{tag=Val} end).

parse(Msg,Ip, Port) ->
	% Drop accidental zeroes - OpenSIPs inserts them sometimes
	% FIXME bug in OpenSIPS?
	[Cookie,C|Rest] = string:tokens([X || X <-  Msg, X /= 0], " ;"),
	Cmd = parse_splitted([string:to_upper(C)|Rest]),
	Cmd#cmd{
		cookie=Cookie,
		origin=#origin{
			type=ser,
			pid=self(),
			ip=Ip,
			port=Port
		}
	}.

encode(Cmd, Answer) when is_list(Answer) ->
	Cmd#cmd.cookie ++ " " ++ Answer ++ "\n";
encode(Cmd, {version, Version}) ->
	Cmd#cmd.cookie ++ " " ++ Version ++ "\n";
encode(Cmd, {supported, _Version}) ->
	Cmd#cmd.cookie ++ ?RTPPROXY_VER_SUPPORTED;
encode(Cmd, ok) ->
	Cmd#cmd.cookie ++ ?RTPPROXY_OK;
encode(Cmd, {error, software}) ->
	Cmd#cmd.cookie ++ ?RTPPROXY_ERR_SOFTWARE;
encode(Cmd, {error, notfound}) ->
	Cmd#cmd.cookie ++ ?RTPPROXY_ERR_NOSESSION;
encode(Msg, {error, syntax}) ->
	[Cookie|_Rest] = string:tokens(Msg, " "),
	Cookie ++ ?RTPPROXY_ERR_SYNTAX;
encode(Cmd, {{I0,I1,I2,I3} = Ip, Port}) when is_integer(I0), is_integer(I1), is_integer(I2), is_integer(I3), is_integer(Port) ->
	Cmd#cmd.cookie ++ " " ++ integer_to_list(Port) ++ " " ++ inet_parse:ntoa(Ip) ++ "\n".

%%
%% Private functions
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
	Params	= parse_params(Args),
	Addr = case {proplists:get_value(direction, Params), is_rfc1918(GuessIp)} of
		{{external, _}, true} -> null;
		{{internal, _}, true} -> {GuessIp, GuessPort};
		{{internal, _}, false} -> null;
		{{external, _}, false} -> {GuessIp, GuessPort}
	end,

	RtcpAddr = case Addr of
		null -> null;
		{GuessIp, GuessPort} -> {GuessIp, GuessPort + 1}
	end,

	#cmd{
		type = ?CMD_U,
		callid = CallId,
		mediaid	= parse_media_id(MediaId),
		from = #party{tag=FromTag, addr=Addr, rtcpaddr=RtcpAddr, proto=proplists:get_value(proto, Params, udp)},
		to = ?SAFE_PARTY(ToTag),
		params= proplists:delete(proto, Params)
	};

% Lookup existing session
% In fact it differs from CMD_U only by the order of tags
parse_splitted([[$L|Args], CallId, ProbableIp, ProbablePort, FromTag, MediaId, ToTag, MediaId]) ->
	parse_splitted([[$U|Args], CallId, ProbableIp, ProbablePort, ToTag, MediaId, FromTag, MediaId]);

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
	parse_splitted(["C", CallId, default, FromTag, "0", null, "0"]);
% Record (obsoleted in favor of Copy)
% No MediaIds
parse_splitted(["R", CallId, FromTag, ToTag]) ->
	parse_splitted(["C", CallId, default, FromTag, "0", ToTag, "0"]);
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


parse_params(A) ->
	parse_params(A, []).

parse_params([], Result) ->
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
			% FIXME is it ok?
			% We just don't care of what client will do with transcoded data at all
			throw({error_syntax, "Requested transcoding but no codecs are available"});
		{Codec, Codecs} ->
			case lists:member(Codec, Codecs) of
				true ->
					R2;
				_ ->
					throw({error_syntax, "Requested transcoding to incompatible codec"})
			end
	end,
	lists:sort(R3);
% IPv6
parse_params([$6|Rest], Result) ->
	parse_params(Rest, ensure_alone(Result, ipv6));
% Asymmetric
parse_params([$A|Rest], Result) ->
	parse_params(Rest, ensure_alone(proplists:delete(symmetric, Result), asymmetric));
% c0,101,100 - Codecs (a bit tricky)
parse_params([$C|Rest], Result) ->
	case string:span(Rest, "0123456789,") of
		0 ->
			% Bogus - skip incomplete modifier
			parse_params(Rest, Result);
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
			parse_params(Rest1, ensure_alone(Result, codecs, Codecs))
	end;
% Direction:
% External (non-RFC1918) network
% Internal (RFC1918) network
% External to External
parse_params([$E, $E|Rest], Result) ->
	parse_params(Rest, ensure_alone(Result, direction, {external, external}));
% External to Internal
parse_params([$E, $I|Rest], Result) ->
	parse_params(Rest, ensure_alone(Result, direction, {external, internal}));
% External to External (single E)
parse_params([$E|Rest], Result) ->
	parse_params(Rest, ensure_alone(Result, direction, {external, external}));
% Internal to External
parse_params([$I, $E|Rest], Result) ->
	parse_params(Rest, ensure_alone(Result, direction, {internal, external}));
% Internal to Internal
parse_params([$I, $I|Rest], Result) ->
	parse_params(Rest, ensure_alone(Result, direction, {internal, internal}));
% Internal to Internal (single I)
parse_params([$I|Rest], Result) ->
	parse_params(Rest, ensure_alone(Result, direction, {internal, internal}));
% l - local address (?)
parse_params([$L|Rest], Result) ->
	parse_params(Rest, ensure_alone(Result, local));
% r - remote address (?)
parse_params([$R|Rest], Result) ->
	parse_params(Rest, ensure_alone(Result, remote));
% Symmetric
parse_params([$S|Rest], Result) ->
	parse_params(Rest, ensure_alone(Result, symmetric));
% Weak
parse_params([$W|Rest], Result) ->
	parse_params(Rest, ensure_alone(Result, weak));
% zNN - repacketization, NN in msec, for the most codecs its value should be
%       in 10ms increments, however for some codecs the increment could differ
%       (e.g. 30ms for GSM or 20ms for G.723).
parse_params([$Z|Rest], Result) ->
	case string:span(Rest, "0123456789") of
		0 ->
			% Bogus - skip incomplete modifier
			parse_params(Rest, Result);
		Ret ->
			Rest1 = string:substr(Rest, Ret + 1),
			{Value, _} = string:to_integer(string:substr(Rest, 1, Ret)),
			parse_params(Rest1, ensure_alone(Result, repacketize, Value))
	end;

%% Extensions

% Protocol - unofficial extension
parse_params([$P, $0 |Rest], Result) ->
	parse_params(Rest, ensure_alone(Result, proto, udp));
parse_params([$P, $1 |Rest], Result) ->
	parse_params(Rest, ensure_alone(Result, proto, tcp));
parse_params([$P, $2 |Rest], Result) ->
	parse_params(Rest, ensure_alone(Result, proto, sctp));
% Transcode - unofficial extension
parse_params([$T|Rest], Result) ->
	case string:span(Rest, "0123456789") of
		0 ->
			% Bogus - skip incomplete modifier
			parse_params(Rest, Result);
		Ret ->
			Rest1 = string:substr(Rest, Ret + 1),
			{Value, _} = string:to_integer(string:substr(Rest, 1, Ret)),
			parse_params(Rest1, ensure_alone(Result, transcode, guess_codec(Value)))
	end;
% Accounting - unofficial extension
parse_params([$V, $0 |Rest], Result) ->
	parse_params(Rest, ensure_alone(Result, acc, start));
parse_params([$V, $1 |Rest], Result) ->
	parse_params(Rest, ensure_alone(Result, acc, interim_update));
parse_params([$V, $2 |Rest], Result) ->
	parse_params(Rest, ensure_alone(Result, acc, stop));

parse_params([_|Rest], Result) ->
	% Unknown parameter - just skip it
	parse_params(Rest, Result).

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

% TODO only IPv4 for now
is_rfc1918({I0,I1,I2,I3} = Ip) when	is_integer(I0), I0 > 0, I0 < 256,
					is_integer(I1), I1 >= 0, I1 < 256,
					is_integer(I2), I2 >= 0, I2 < 256,
					is_integer(I3), I3 >= 0, I3 < 256
				->
	is_rfc1918_guarded(Ip);
is_rfc1918(_) ->
	throw({error, "Not a valid IPv4 address"}).

% Loopback (actually, it's not a RFC1918 network)
is_rfc1918_guarded({127,_,_,_}) ->
	true;
% RFC 1918, 10.0.0.0 - 10.255.255.255
is_rfc1918_guarded({10,_,_,_}) ->
	true;
% RFC 1918, 172.16.0.0 - 172.31.255.255
is_rfc1918_guarded({172,I1,_,_}) when I1 > 15, I1 < 32 ->
	true;
% RFC 1918, 192.168.0.0 - 192.168.255.255
is_rfc1918_guarded({192,168,_,_}) ->
	true;
is_rfc1918_guarded(_) ->
	false.
