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
	[Cookie|Rest] = string:tokens(Msg, " ;"),
	Cmd = parse_splitted(Rest),
	Cmd#cmd{
		cookie=Cookie,
		origin=#origin{
			type=ser,
			pid=self(),
			ip=Ip,
			port=Port
		}
	}.

parse_splitted(["V"]) ->
	% Request basic supported rtpproxy protocol version
	#cmd{
		type=?CMD_V
	};

parse_splitted(["VF" | Params]) ->
	% Request additional rtpproxy protocol extensions
	#cmd{
		type=?CMD_VF,
		params=Params
	};

parse_splitted([[$U|Args], CallId, ProbableIp, ProbablePort, FromTag, FromMediaId]) ->
	% update/create session
	{GuessIp, GuessPort} = parse_addr(ProbableIp, ProbablePort),
	#cmd{
		type=?CMD_U,
		callid=CallId,
		addr={GuessIp, GuessPort},
		from={FromTag, parse_media_id(FromMediaId)},
		params=parse_params(Args)
	};

parse_splitted([[$U|Args], CallId, ProbableIp, ProbablePort, FromTag, FromMediaId, ToTag, ToMediaId]) ->
	% Reinvite, Hold and Resume
	{GuessIp, GuessPort} = parse_addr(ProbableIp, ProbablePort),
	#cmd{
		type=?CMD_U,
		callid=CallId,
		addr={GuessIp, GuessPort},
		from={FromTag, parse_media_id(FromMediaId)},
		to={ToTag, parse_media_id(ToMediaId)},
		params=parse_params(Args)
	};

parse_splitted([[$L|Args], CallId, ProbableIp, ProbablePort, FromTag, FromMediaId, ToTag, ToMediaId]) ->
	% lookup existing session
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

parse_splitted([[$D|Args], CallId, FromTag]) ->
	% delete session (no MediaIds and no ToTag)
	#cmd{
		type=?CMD_D,
		callid=CallId,
		from={FromTag, 0},
		params=parse_params(Args)
	};

parse_splitted([[$D|Args], CallId, FromTag, ToTag]) ->
	% delete session (no MediaIds)
	#cmd{
		type=?CMD_D,
		callid=CallId,
		from={FromTag, 0},
		to={ToTag, 0},
		params=parse_params(Args)
	};

parse_splitted(["R", CallId, FromTag, FromMediaId, ToTag, ToMediaId]) ->
	% record (obsoleted)
	% TODO should both MediaIds be equal?
	#cmd{
		type=?CMD_R,
		callid=CallId,
		from={FromTag, parse_media_id(FromMediaId)},
		to={ToTag, parse_media_id(ToMediaId)}
	};

parse_splitted([[$P|Args], CallId, PlayName, Codecs, FromTag, FromMediaId, ToTag, ToMediaId]) ->
	% playback pre-recorded audio
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
parse_splitted([[$P|Args], CallId, PlayName, Codecs, FromTag, FromMediaId, ToTag, ToMediaId, ProbableIp, ProbablePort]) ->
	% playback pre-recorded audio (Hold and Resume)
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

parse_splitted(["S", CallId, FromTag, FromMediaId, ToTag, ToMediaId]) ->
	% stop playback or record
	% TODO should both MediaIds be equal?
	#cmd{
		type=?CMD_S,
		callid=CallId,
		from={FromTag, parse_media_id(FromMediaId)},
		to={ToTag, parse_media_id(ToMediaId)}
	};

parse_splitted(["C", CallId, RecordName, FromTag, FromMediaId, ToTag, ToMediaId]) ->
	% copy session (same as record?)
	% TODO should both MediaIds be equal?
	#cmd{
		type=?CMD_C,
		callid=CallId,
		from={FromTag, parse_media_id(FromMediaId)},
		to={ToTag, parse_media_id(ToMediaId)},
		filename=RecordName
	};

parse_splitted(["Q", CallId, FromTag, FromMediaId, ToTag, ToMediaId]) ->
	% query
	% TODO should both MediaIds be equal?
	#cmd{
		type=?CMD_Q,
		callid=CallId,
		from={FromTag, parse_media_id(FromMediaId)},
		to={ToTag, parse_media_id(ToMediaId)}
	};

parse_splitted(["X"]) ->
	% stop all active sessions
	#cmd{
		type=?CMD_X
	};

parse_splitted(["I"]) ->
	% Get overall statistics
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
	{Mod, _} = lists:unzip(lists:filter(fun({_, Sym}) -> lists:member(Sym, A) end, ?MOD_LIST)),
	Mod.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% TODO

-endif.

