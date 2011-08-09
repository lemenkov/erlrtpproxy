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

-include("common.hrl").

-define(RTPPROXY_OK, " 0\n").
-define(RTPPROXY_VER_SUPPORTED, " 1\n").
-define(RTPPROXY_ERR_SYNTAX,    " E1\n").
-define(RTPPROXY_ERR_SOFTWARE,  " E7\n").
-define(RTPPROXY_ERR_NOSESSION, " E8\n").

-define(SAFE_PARTY(Val), case Val of null -> null; _ -> #party{tag=Val} end).

parse(Msg,Ip, Port) ->
	% TODO pass modifiers as atoms (not as string)
	[Cookie,C|Rest] = string:tokens(Msg, " ;"),
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
encode(Cmd, {error, notfound}) ->
	Cmd#cmd.cookie ++ ?RTPPROXY_ERR_NOSESSION;
encode(Cookie, {error, syntax}) ->
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

% Create session (no ToTag)
parse_splitted([[$U|Args], CallId, ProbableIp, ProbablePort, FromTag, MediaId]) ->
	parse_splitted([[$U|Args], CallId, ProbableIp, ProbablePort, FromTag, MediaId, null, MediaId]);
% Reinvite, Hold and Resume
parse_splitted([[$U|Args], CallId, ProbableIp, ProbablePort, FromTag, MediaId, ToTag, MediaId]) ->
	{GuessIp, GuessPort} = parse_addr(ProbableIp, ProbablePort),
	#cmd{
		type = ?CMD_U,
		callid = CallId,
		mediaid	= parse_media_id(MediaId),
		from = #party{tag=FromTag, addr={GuessIp, GuessPort}},
		to = ?SAFE_PARTY(ToTag),
		params	= parse_params(Args)
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
		params=parse_playcount(Args) ++ [{filename, PlayName}, {codecs, Codecs}]
	};
% Playback pre-recorded audio (Music-on-hold and resume)
parse_splitted([[$P|Args], CallId, PlayName, Codecs, FromTag, MediaId, ToTag, MediaId]) ->
	#cmd{
		type=?CMD_P,
		callid=CallId,
		mediaid=parse_media_id(MediaId),
		from=#party{tag=FromTag},
		to=#party{tag=ToTag},
		params=parse_playcount(Args) ++ [{filename, PlayName}, {codecs, Codecs}]
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
		params=parse_playcount(Args) ++ [{filename, PlayName}, {codecs, Codecs}, {addr, {GuessIp, GuessPort}}]
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
	Result;
% Asymmetric
parse_params([$A|Rest], Result) ->
	parse_params(Rest, Result ++ [asymmetric]);
% c0,101,100 - Codecs (a bit tricky)
parse_params([$C|Rest], Result) ->
	case string:span(Rest, "0123456789,") of
		0 ->
			% Bogus - skip incomplete modifier
			parse_params(Rest, Result);
		Ret ->
			Rest1 = string:substr(Rest, Ret + 1),
			% FIXME use atoms instead of numbers where possible
			% grep "a=rtpmap:" /var/log/messages | sed -e 's,.*a=rtpmap:,,g' | sort | uniq | sort -n
			% http://www.iana.org/assignments/rtp-parameters
			% http://www.iana.org/assignments/media-types/audio/index.html
			Codecs = lists:map(fun(X) -> {Y, _} = string:to_integer(X), Y end, string:tokens(string:substr(Rest, 1, Ret), ",")),
			parse_params(Rest1, Result ++ [{codecs, Codecs}])
	end;
% IPv6
parse_params([$6|Rest], Result) ->
	parse_params(Rest, Result ++ [ipv6]);
% Internal (RFC1918) network
parse_params([$I|Rest], Result) ->
	parse_params(Rest, Result ++ [internal]);
% External (non-RFC1918) network
parse_params([$E|Rest], Result) ->
	parse_params(Rest, Result ++ [external]);
% Symmetric
parse_params([$S|Rest], Result) ->
	parse_params(Rest, Result ++ [symmetric]);
% Weak
parse_params([$W|Rest], Result) ->
	parse_params(Rest, Result ++ [weak]);
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
			Value = string:to_integer(string:substr(Rest, 1, Ret)),
			parse_params(Rest1, Result ++ [{repacketize, Value}])
	end;
% Lock
parse_params([$L|Rest], Result) ->
	parse_params(Rest, Result ++ [lock]);
% r - remote address (?)
parse_params([_|Rest], Result) ->
	% Unknown parameter - just skip it
	parse_params(Rest, Result).

%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_cmd_v_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_V,
			cookie="24390_0",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234}
		}, parse("24390_0 V", {127,0,0,1}, 1234)).

parse_cmd_vf_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_VF,
			cookie="24393_1",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			params="20050322"
		}, parse("24393_1 VF 20050322", {127,0,0,1}, 1234)).

parse_cmd_u_1_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="24393_4",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="0003e30c-c50c00f7-123e8bd9-542f2edf@192.168.0.100",
			mediaid=1,
			from=#party{tag="0003e30cc50cd69210b8c36b-0ecf0120",addr={{192,168,0,100}, 27686}},
			params=[{codecs,[0,8,18,101]}]
		}, parse("24393_4 Uc0,8,18,101 0003e30c-c50c00f7-123e8bd9-542f2edf@192.168.0.100 192.168.0.100 27686 0003e30cc50cd69210b8c36b-0ecf0120;1", {127,0,0,1}, 1234)).

parse_cmd_u_2_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="438_41061",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="e12ea248-94a5e885@192.168.5.3",
			mediaid=1,
			from=#party{tag="6b0a8f6cfc543db1o1",addr={{192,168,5,3}, 16432}},
			params=[{codecs,[8,0,2,4,18,96,97,98,100,101]}]
		}, parse("438_41061 Uc8,0,2,4,18,96,97,98,100,101 e12ea248-94a5e885@192.168.5.3 192.168.5.3 16432 6b0a8f6cfc543db1o1;1", {127,0,0,1}, 1234)).

parse_cmd_l_1_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="413_40797",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="452ca314-3bbcf0ea@192.168.0.2",
			mediaid=1,
			from=#party{tag="8d11d16a3b56fcd588d72b3d359cc4e1",addr={{192,168,100,4}, 17050}},
			to=#party{tag="e4920d0cb29cf52o0"},
			params=[{codecs,[0,101,100]}]
		}, parse("413_40797 Lc0,101,100 452ca314-3bbcf0ea@192.168.0.2 192.168.100.4 17050 e4920d0cb29cf52o0;1 8d11d16a3b56fcd588d72b3d359cc4e1;1", {127,0,0,1}, 1234)).

parse_cmd_l_2_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="418_41111",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="a68e961-5f6a75e5-356cafd9-3562@192.168.100.6",
			mediaid=1,
			from=#party{tag="60753eabbd87fe6f34068e9d80a9fc1c",addr={{192,168,100,4}, 18756}},
			to=#party{tag="1372466422"},
			params=[internal, {codecs,[8,101,100]}]
		}, parse("418_41111 LIc8,101,100 a68e961-5f6a75e5-356cafd9-3562@192.168.100.6 192.168.100.4 18756 1372466422;1 60753eabbd87fe6f34068e9d80a9fc1c;1", {127,0,0,1}, 1234)).

parse_cmd_d_1_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_D,
			cookie="441_40922",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="2498331773@192.168.1.37",
			mediaid=0,
			from=#party{tag="8edccef4eb1a16b8cef7192b77b7951a"}
		}, parse("441_40922 D 2498331773@192.168.1.37 8edccef4eb1a16b8cef7192b77b7951a", {127,0,0,1}, 1234)).

parse_cmd_d_2_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_D,
			cookie="437_40882",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="7adc6214-268583a6-1b74a438-3548@192.168.100.6",
			mediaid=0,
			from=#party{tag="1372466422"},
			to=#party{tag="9c56ba15bd794082ce6b166dba6c9c2"}
		}, parse("437_40882 D 7adc6214-268583a6-1b74a438-3548@192.168.100.6 1372466422 9c56ba15bd794082ce6b166dba6c9c2", {127,0,0,1}, 1234)).


parse_cmd_r_1_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_C,
			cookie="393_6",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="0003e348-e21901f6-29cc58a1-379f3ffd@192.168.0.1",
			mediaid=0,
			from=#party{tag="0003e348e219767510f1e38f-47c56231"},
			to=null,
			params=[{filename, default}]
		}, parse("393_6 R 0003e348-e21901f6-29cc58a1-379f3ffd@192.168.0.1 0003e348e219767510f1e38f-47c56231", {127,0,0,1}, 1234)).

parse_cmd_r_2_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_C,
			cookie="32711_5",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="0003e30c-c50c016a-35dc4387-58a65654@192.168.0.100",
			mediaid=0,
			from=#party{tag="eb1f1ca7e74cf0fc8a81ea331486452a"},
			to=#party{tag="0003e30cc50ccbed0342cc8d-0bddf550"},
			params=[{filename, default}]
		}, parse("32711_5 R 0003e30c-c50c016a-35dc4387-58a65654@192.168.0.100 eb1f1ca7e74cf0fc8a81ea331486452a 0003e30cc50ccbed0342cc8d-0bddf550", {127,0,0,1}, 1234)).

parse_cmd_p_1_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_P,
			cookie="2154_5",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="0003e30c-c50c0171-35b90751-013a3ef6@192.168.0.100",
			mediaid=1,
			from=#party{tag="0003e30cc50ccc9f743d4fa6-38d0bd14"},
			to=null,
			params=[{playcount, 20}, {filename,"/var/run/tmp/hello_uac.wav"},{codecs,"session"}]
		}, parse("2154_5 P20 0003e30c-c50c0171-35b90751-013a3ef6@192.168.0.100 /var/run/tmp/hello_uac.wav session 0003e30cc50ccc9f743d4fa6-38d0bd14;1", {127,0,0,1}, 1234)).

parse_cmd_p_2_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_P,
			cookie="1389_5",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="0003e30c-c50c016d-46bbcf2e-6369eecf@192.168.0.100",
			mediaid=1,
			from=#party{tag="0003e30cc50ccc5416857d59-357336dc"},
			to=#party{tag="28d49e51a95d5a31d09b31ccc63c5f4b"},
			params=[{playcount, 10}, {filename,"/var/tmp/rtpproxy_test/media/01.wav"},{codecs,"session"}]
		}, parse("1389_5 P10 0003e30c-c50c016d-46bbcf2e-6369eecf@192.168.0.100 /var/tmp/rtpproxy_test/media/01.wav session 0003e30cc50ccc5416857d59-357336dc;1 28d49e51a95d5a31d09b31ccc63c5f4b;1", {127,0,0,1}, 1234)).

parse_cmd_s_1_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_S,
			cookie="2154_6",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="0003e30c-c50c0171-35b90751-013a3ef6@192.168.0.100",
			mediaid=1,
			from=#party{tag="0003e30cc50ccc9f743d4fa6-38d0bd14"},
			to=null
		}, parse("2154_6 S 0003e30c-c50c0171-35b90751-013a3ef6@192.168.0.100 0003e30cc50ccc9f743d4fa6-38d0bd14;1", {127,0,0,1}, 1234)).

encode_ok_test() ->
	?assertEqual("438_41067 0\n", encode(#cmd{cookie="438_41067"}, ok)).

encode_ip_test() ->
	?assertEqual("8411_41413 41212 192.168.100.4\n", encode(#cmd{cookie="8411_41413"}, {{192,168,100,4},41212})).

-endif.

