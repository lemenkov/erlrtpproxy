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
parse_splitted([[$U|Args], CallId, ProbableIp, ProbablePort, FromTag, FromMediaId]) ->
	{GuessIp, GuessPort} = parse_addr(ProbableIp, ProbablePort),
	#cmd{
		type=?CMD_U,
		callid=CallId,
		addr={GuessIp, GuessPort},
		from={FromTag, parse_media_id(FromMediaId)},
		params=parse_params(Args)
	};

% Reinvite, Hold and Resume
parse_splitted([[$U|Args], CallId, ProbableIp, ProbablePort, FromTag, FromMediaId, ToTag, ToMediaId]) ->
	{GuessIp, GuessPort} = parse_addr(ProbableIp, ProbablePort),
	#cmd{
		type=?CMD_U,
		callid=CallId,
		addr={GuessIp, GuessPort},
		from={FromTag, parse_media_id(FromMediaId)},
		to={ToTag, parse_media_id(ToMediaId)},
		params=parse_params(Args)
	};

% Lookup existing session
parse_splitted([[$L|Args], CallId, ProbableIp, ProbablePort, FromTag, FromMediaId, ToTag, ToMediaId]) ->
	% TODO should both MediaIds be equal?
	{GuessIp, GuessPort} = parse_addr(ProbableIp, ProbablePort),
	#cmd{
		type=?CMD_L,
		callid=CallId,
		addr={GuessIp, GuessPort},
		from={FromTag, parse_media_id(FromMediaId)},
		to={ToTag, parse_media_id(ToMediaId)},
		params=parse_params(Args)
	};

% delete session (no MediaIds and no ToTag) - Cancel
parse_splitted([[$D|Args], CallId, FromTag]) ->
	#cmd{
		type=?CMD_D,
		callid=CallId,
		from={FromTag, 0},
		params=parse_params(Args)
	};

% delete session (no MediaIds) - Bye
parse_splitted([[$D|Args], CallId, FromTag, ToTag]) ->
	#cmd{
		type=?CMD_D,
		callid=CallId,
		from={FromTag, 0},
		to={ToTag, 0},
		params=parse_params(Args)
	};

% Record (obsoleted)
parse_splitted(["R", CallId, FromTag, FromMediaId, ToTag, ToMediaId]) ->
	% TODO should both MediaIds be equal?
	#cmd{
		type=?CMD_R,
		callid=CallId,
		from={FromTag, parse_media_id(FromMediaId)},
		to={ToTag, parse_media_id(ToMediaId)}
	};

% Playback pre-recorded audio (Music-on-hold and resume)
parse_splitted([[$P|Args], CallId, PlayName, Codecs, FromTag, FromMediaId, ToTag, ToMediaId]) ->
	% TODO should both MediaIds be equal?
	#cmd{
		type=?CMD_P,
		callid=CallId,
		from={FromTag, parse_media_id(FromMediaId)},
		to={ToTag, parse_media_id(ToMediaId)},
		params=parse_params(Args),
		filename=PlayName,
		codecs=Codecs
	};
% Playback pre-recorded audio (Music-on-hold and resume)
parse_splitted([[$P|Args], CallId, PlayName, Codecs, FromTag, FromMediaId, ToTag, ToMediaId, ProbableIp, ProbablePort]) ->
	% TODO should both MediaIds be equal?
	{GuessIp, GuessPort} = parse_addr(ProbableIp, ProbablePort),
	#cmd{
		type=?CMD_P,
		callid=CallId,
		from={FromTag, parse_media_id(FromMediaId)},
		to={ToTag, parse_media_id(ToMediaId)},
		params=parse_params(Args),
		filename=PlayName,
		codecs=Codecs,
		addr={GuessIp, GuessPort}
	};

% Stop playback or record
parse_splitted(["S", CallId, FromTag, FromMediaId, ToTag, ToMediaId]) ->
	% TODO should both MediaIds be equal?
	#cmd{
		type=?CMD_S,
		callid=CallId,
		from={FromTag, parse_media_id(FromMediaId)},
		to={ToTag, parse_media_id(ToMediaId)}
	};

% Copy session (same as record, which is now obsolete)
parse_splitted(["C", CallId, RecordName, FromTag, FromMediaId, ToTag, ToMediaId]) ->
	% TODO should both MediaIds be equal?
	#cmd{
		type=?CMD_C,
		callid=CallId,
		from={FromTag, parse_media_id(FromMediaId)},
		to={ToTag, parse_media_id(ToMediaId)},
		filename=RecordName
	};

% Query information about one particular session
parse_splitted(["Q", CallId, FromTag, FromMediaId, ToTag, ToMediaId]) ->
	% TODO should both MediaIds be equal?
	#cmd{
		type=?CMD_Q,
		callid=CallId,
		from={FromTag, parse_media_id(FromMediaId)},
		to={ToTag, parse_media_id(ToMediaId)}
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

% a - asymmetric
% e - external network
% i - internal (RFC1918) network
% 6 - IPv6
% s - symmetric
% w - weak
% zNN - repacketization, NN in msec, for the most codecs its value should be
%       in 10ms increments, however for some codecs the increment could differ
%       (e.g. 30ms for GSM or 20ms for G.723).
% c - codecs
% l - lock
% r - remote address (?)
parse_params(A) ->
	{Mod, _} = lists:unzip(lists:filter(fun({_, Sym}) -> lists:member(Sym, A) end, ?MOD_LIST)),
	Mod.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% TODO
% 24390_0 V
% 24393_1 VF 20050322
% 24393_4 Uc0,8,18,101 0003e30c-c50c00f7-123e8bd9-542f2edf@192.168.0.100 192.168.0.100 27686 0003e30cc50cd69210b8c36b-0ecf0120;1
% 438_41061 Uc8,0,2,4,18,96,97,98,100,101 e12ea248-94a5e885@192.168.5.3 192.168.5.3 16432 6b0a8f6cfc543db1o1;1
% 413_40797 Lc0,101,100 452ca314-3bbcf0ea@192.168.0.2 192.168.100.4 17050 e4920d0cb29cf52o0;1 8d11d16a3b56fcd588d72b3d359cc4e1;1
% 418_41111 LIc8,101,100 a68e961-5f6a75e5-356cafd9-3562@192.168.100.6 192.168.100.4 18756 1372466422;1 60753eabbd87fe6f34068e9d80a9fc1c;1
% 441_40922 D 2498331773@192.168.1.37 8edccef4eb1a16b8cef7192b77b7951a 1372466422
% 437_40882 D 7adc6214-268583a6-1b74a438-3548@192.168.100.6 1372466422 9c56ba15bd794082ce6b166dba6c9c2
% E448_40701 D 255765531@10.10.10.19 953707145

-endif.

