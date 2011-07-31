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

-include("common.hrl").

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

% Update / create session
parse_splitted([[$U|Args], CallId, ProbableIp, ProbablePort, FromTag, MediaId]) ->
	{GuessIp, GuessPort} = parse_addr(ProbableIp, ProbablePort),
	#cmd{
		type=?CMD_U,
		callid=CallId,
		mediaid=parse_media_id(MediaId),
		addr={GuessIp, GuessPort},
		from={FromTag},
		params=parse_params(Args)
	};

% Reinvite, Hold and Resume
parse_splitted([[$U|Args], CallId, ProbableIp, ProbablePort, FromTag, MediaId, ToTag, MediaId]) ->
	{GuessIp, GuessPort} = parse_addr(ProbableIp, ProbablePort),
	#cmd{
		type=?CMD_U,
		callid=CallId,
		mediaid=parse_media_id(MediaId),
		addr={GuessIp, GuessPort},
		from={FromTag},
		to={ToTag},
		params=parse_params(Args)
	};

% Lookup existing session
parse_splitted([[$L|Args], CallId, ProbableIp, ProbablePort, FromTag, MediaId, ToTag, MediaId]) ->
	{GuessIp, GuessPort} = parse_addr(ProbableIp, ProbablePort),
	#cmd{
		type=?CMD_L,
		callid=CallId,
		mediaid=parse_media_id(MediaId),
		addr={GuessIp, GuessPort},
		from={FromTag},
		to={ToTag},
		params=parse_params(Args)
	};

% delete session (no MediaIds and no ToTag) - Cancel
parse_splitted(["D", CallId, FromTag]) ->
	#cmd{
		type=?CMD_D,
		callid=CallId,
		from={FromTag}
	};

% delete session (no MediaIds) - Bye
parse_splitted(["D", CallId, FromTag, ToTag]) ->
	#cmd{
		type=?CMD_D,
		callid=CallId,
		from={FromTag},
		to={ToTag}
	};

% Record (obsoleted)
parse_splitted(["R", CallId, FromTag, MediaId, ToTag, MediaId]) ->
	#cmd{
		type=?CMD_R,
		callid=CallId,
		mediaid=parse_media_id(MediaId),
		from={FromTag},
		to={ToTag}
	};

% Playback pre-recorded audio (Music-on-hold and resume)
parse_splitted([[$P|Args], CallId, PlayName, Codecs, FromTag, MediaId, ToTag, MediaId]) ->
	#cmd{
		type=?CMD_P,
		callid=CallId,
		mediaid=parse_media_id(MediaId),
		from={FromTag},
		to={ToTag},
		params=parse_params(Args),
		filename=PlayName,
		codecs=Codecs
	};
% Playback pre-recorded audio (Music-on-hold and resume)
parse_splitted([[$P|Args], CallId, PlayName, Codecs, FromTag, MediaId, ToTag, MediaId, ProbableIp, ProbablePort]) ->
	{GuessIp, GuessPort} = parse_addr(ProbableIp, ProbablePort),
	#cmd{
		type=?CMD_P,
		callid=CallId,
		mediaid=parse_media_id(MediaId),
		from={FromTag},
		to={ToTag},
		params=parse_params(Args),
		filename=PlayName,
		codecs=Codecs,
		addr={GuessIp, GuessPort}
	};

% Stop playback or record
parse_splitted(["S", CallId, FromTag, MediaId, ToTag, MediaId]) ->
	#cmd{
		type=?CMD_S,
		callid=CallId,
		mediaid=parse_media_id(MediaId),
		from={FromTag},
		to={ToTag}
	};

% Copy session (same as record, which is now obsolete)
parse_splitted(["C", CallId, RecordName, FromTag, MediaId, ToTag, MediaId]) ->
	#cmd{
		type=?CMD_C,
		callid=CallId,
		mediaid=parse_media_id(MediaId),
		from={FromTag},
		to={ToTag},
		filename=RecordName
	};

% Query information about one particular session
parse_splitted(["Q", CallId, FromTag, MediaId, ToTag, MediaId]) ->
	#cmd{
		type=?CMD_Q,
		callid=CallId,
		mediaid=parse_media_id(MediaId),
		from={FromTag},
		to={ToTag}
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

parse_media_id(ProbableMediaId) ->
	try list_to_integer (ProbableMediaId)
	catch
		_:_ ->
			throw({error_syntax, "Wrong MediaID"})
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

cmd_v_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_V,
			cookie="24390_0",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234}
		}, parse("24390_0 V", {127,0,0,1}, 1234)).

cmd_vf_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_VF,
			cookie="24393_1",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			params="20050322"
		}, parse("24393_1 VF 20050322", {127,0,0,1}, 1234)).

cmd_u_1_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="24393_4",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="0003e30c-c50c00f7-123e8bd9-542f2edf@192.168.0.100",
			addr={{192,168,0,100}, 27686},
			from={"0003e30cc50cd69210b8c36b-0ecf0120",1},
			params=[{codecs,[0,8,18,101]}]
		}, parse("24393_4 Uc0,8,18,101 0003e30c-c50c00f7-123e8bd9-542f2edf@192.168.0.100 192.168.0.100 27686 0003e30cc50cd69210b8c36b-0ecf0120;1", {127,0,0,1}, 1234)).

cmd_u_2_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="438_41061",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="e12ea248-94a5e885@192.168.5.3",
			addr={{192,168,5,3}, 16432},
			from={"6b0a8f6cfc543db1o1",1},
			params=[{codecs,[8,0,2,4,18,96,97,98,100,101]}]
		}, parse("438_41061 Uc8,0,2,4,18,96,97,98,100,101 e12ea248-94a5e885@192.168.5.3 192.168.5.3 16432 6b0a8f6cfc543db1o1;1", {127,0,0,1}, 1234)).

cmd_l_1_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_L,
			cookie="413_40797",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="452ca314-3bbcf0ea@192.168.0.2",
			addr={{192,168,100,4}, 17050},
			from={"e4920d0cb29cf52o0",1},
			to={"8d11d16a3b56fcd588d72b3d359cc4e1",1},
			params=[{codecs,[0,101,100]}]
		}, parse("413_40797 Lc0,101,100 452ca314-3bbcf0ea@192.168.0.2 192.168.100.4 17050 e4920d0cb29cf52o0;1 8d11d16a3b56fcd588d72b3d359cc4e1;1", {127,0,0,1}, 1234)).

cmd_l_2_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_L,
			cookie="418_41111",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="a68e961-5f6a75e5-356cafd9-3562@192.168.100.6",
			addr={{192,168,100,4}, 18756},
			from={"1372466422",1},
			to={"60753eabbd87fe6f34068e9d80a9fc1c",1},
			params=[internal, {codecs,[8,101,100]}]
		}, parse("418_41111 LIc8,101,100 a68e961-5f6a75e5-356cafd9-3562@192.168.100.6 192.168.100.4 18756 1372466422;1 60753eabbd87fe6f34068e9d80a9fc1c;1", {127,0,0,1}, 1234)).

cmd_d_1_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_D,
			cookie="441_40922",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="2498331773@192.168.1.37",
			from={"8edccef4eb1a16b8cef7192b77b7951a",0}
		}, parse("441_40922 D 2498331773@192.168.1.37 8edccef4eb1a16b8cef7192b77b7951a", {127,0,0,1}, 1234)).

cmd_d_2_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_D,
			cookie="437_40882",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="7adc6214-268583a6-1b74a438-3548@192.168.100.6",
			from={"1372466422",0},
			to={"9c56ba15bd794082ce6b166dba6c9c2", 0}
		}, parse("437_40882 D 7adc6214-268583a6-1b74a438-3548@192.168.100.6 1372466422 9c56ba15bd794082ce6b166dba6c9c2", {127,0,0,1}, 1234)).

-endif.

