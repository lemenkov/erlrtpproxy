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

-module(ser).
-author('lemenkov@gmail.com').

-behaviour(gen_server).
-export([start/1]).
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include("common.hrl").

start(Args) ->
	gen_server:start(?MODULE, Args, []).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

init ({Ip, Port}) ->
	process_flag(trap_exit, true),
	case gen_udp:open(Port, [{ip, Ip}, {active, true}, list]) of
		{ok, Fd} ->
			?PRINT("started at [~s:~w]", [inet_parse:ntoa(Ip), Port]),
			{ok, Fd};
		{error, Reason} ->
			?PRINT("interface not started. Reason [~p]", [Reason]),
			{stop, Reason}
	end.

handle_call(_Other, _From, Fd) ->
	{noreply, Fd}.

handle_cast(_Request, Fd) ->
	{noreply, Fd}.

code_change(_OldVsn, Fd, _Extra) ->
	{ok, Fd}.

terminate(Reason, Fd) ->
	gen_udp:close(Fd),
	?PRINT("thread terminated due to reason [~p]", [Reason]).

% Fd from which message arrived must be equal to Fd from our state
% Brief introbuction of protocol is here: http://rtpproxy.org/wiki/RTPproxyProtocol
handle_info({udp, Fd, Ip, Port, Msg}, Fd) ->
	% TODO fix issue with missing Cookie
	% TODO pass modifiers as atoms (not as string)
	[Cookie|Rest] = string:tokens(Msg, " ;"),
	?PRINT("SER cmd: ~p", [Rest]),

	ParseAddr = fun (ProbableIp, ProbablePort) ->
		try inet_parse:address(ProbableIp) of
			{ok, GuessIp} ->
				try list_to_integer(ProbablePort) of
					GuessPort when GuessPort >= 0, GuessPort < 65536 ->
						{GuessIp, GuessPort};
					_ ->
						{error, "Wrong port"}
				catch
					_:_ ->
						{error, "Wrong port"}
				end
		catch
			_:_ ->
				{error, "Wrong IP"}
		end
	end,
	Answer = case
		case Rest of
			% Request basic supported rtpproxy protocol version
			["V"] ->
				#cmd{cookie=Cookie, type=?CMD_V};
			% Request additional rtpproxy protocol extensions
			["VF", Params] ->
				#cmd{cookie=Cookie, type=?CMD_VF, params=Params};
			% update/create session
			[[$U|Args], CallId, OrigIp, OrigPort, FromTag, FromMediaId|To] ->
				{Modifiers, _} = lists:unzip(lists:filter(fun({_, Sym}) -> lists:member(Sym, Args) end, ?MOD_LIST)),
				case ParseAddr(OrigIp, OrigPort) of
					{error, ErrMsg} ->
						?PRINT("Error: ~p", [ErrMsg]),
						error_syntax;
					{GuessIp, GuessPort} ->
						case To of
							[] ->
								#cmd{cookie=Cookie, type=?CMD_U, callid=CallId, addr={GuessIp, GuessPort}, from={FromTag, list_to_integer(FromMediaId)}, params=Modifiers};
							[ToTag,ToMediaId] ->
								% Reinvite, Hold and Resume
								#cmd{cookie=Cookie, type=?CMD_U, callid=CallId, addr={GuessIp, GuessPort}, from={FromTag, list_to_integer(FromMediaId)}, to={ToTag, list_to_integer(ToMediaId)}, params=Modifiers}
						end
				end;
			% lookup existing session
			% TODO should both MediaIds be equal?
			[[$L|Args], CallId, OrigIp, OrigPort, FromTag, FromMediaId, ToTag, ToMediaId] ->
				{Modifiers, _} = lists:unzip(lists:filter(fun({_, Sym}) -> lists:member(Sym, Args) end, ?MOD_LIST)),
				case ParseAddr(OrigIp, OrigPort) of
					{error, ErrMsg} ->
						?PRINT("Error: ~p", [ErrMsg]),
						error_syntax;
					{GuessIp, GuessPort} ->
						#cmd{cookie=Cookie, type=?CMD_L, callid=CallId, addr={GuessIp, GuessPort}, from={FromTag, list_to_integer(FromMediaId)}, to={ToTag, list_to_integer(ToMediaId)}, params=Modifiers}
				end;
			% delete session (no MediaIds)
			[[$D|Args], CallId, FromTag, ToTag] ->
				{Modifiers, _} = lists:unzip(lists:filter(fun({_, Sym}) -> lists:member(Sym, Args) end, ?MOD_LIST)),
				#cmd{cookie=Cookie, type=?CMD_D, callid=CallId, from={FromTag, 0}, to={ToTag, 0}, params=Modifiers};
			% record (obsoleted)
			% TODO should both MediaIds be equal?
			["R", CallId, FromTag, FromMediaId, ToTag, ToMediaId] ->
				#cmd{cookie=Cookie, type=?CMD_R, callid=CallId, from={FromTag, list_to_integer(FromMediaId)}, to={ToTag, list_to_integer(ToMediaId)}};
			% playback pre-recorded audio
			% TODO should both MediaIds be equal?
			[[$P|Args], CallId, PlayName, Codecs, FromTag, FromMediaId, ToTag, ToMediaId|Addr] ->
				case Addr of
					[] ->
						#cmd{cookie=Cookie, type=?CMD_P, callid=CallId, from={FromTag, list_to_integer(FromMediaId)}, to={ToTag, list_to_integer(ToMediaId)}, filename=PlayName, codecs=Codecs};
					[OrigIp,OrigPort] ->
						% Hold and Resume
						case ParseAddr(OrigIp, OrigPort) of
							{error, ErrMsg} ->
								?PRINT("Error: ~p", [ErrMsg]),
								error_syntax;
							{GuessIp, GuessPort} ->
								#cmd{cookie=Cookie, type=?CMD_P, callid=CallId, from={FromTag, list_to_integer(FromMediaId)}, to={ToTag, list_to_integer(ToMediaId)}, filename=PlayName, codecs=Codecs, addr={GuessIp, GuessPort}}
						end
				end;
			% stop playback or record
			% TODO should both MediaIds be equal?
			["S", CallId, FromTag, FromMediaId, ToTag, ToMediaId] ->
				#cmd{cookie=Cookie, type=?CMD_S, callid=CallId, from={FromTag, list_to_integer(FromMediaId)}, to={ToTag, list_to_integer(ToMediaId)}};
			% copy session (same as record?)
			% TODO should both MediaIds be equal?
			["C", CallId, RecordName, FromTag, FromMediaId, ToTag, ToMediaId] ->
				#cmd{cookie=Cookie, type=?CMD_C, callid=CallId, from={FromTag, list_to_integer(FromMediaId)}, to={ToTag, list_to_integer(ToMediaId)}, filename=RecordName};
			% query
			% TODO should both MediaIds be equal?
			["Q", CallId, FromTag, FromMediaId, ToTag, ToMediaId] ->
				#cmd{cookie=Cookie, type=?CMD_Q, callid=CallId, from={FromTag, list_to_integer(FromMediaId)}, to={ToTag, list_to_integer(ToMediaId)}};
			% stop all active sessions
			["X"] ->
				#cmd{cookie=Cookie, type=?CMD_X};
			["I"] ->
				#cmd{cookie=Cookie, type=?CMD_I};
			_Other ->
				error_syntax
		end
	of
		error_syntax ->
			?PRINT("Other command (bad syntax?) [~s]", [Msg]),
			?RTPPROXY_ERR_SYNTAX;
		Cmd ->
			gen_server:call({global, rtpproxy}, {message, Cmd})

	end,
	gen_udp:send(Fd, Ip, Port, Cookie ++ " " ++  Answer ++ "\n"),
	{noreply, Fd};

handle_info(Info, Fd) ->
	?PRINT("Info [~w]", [Info]),
	{noreply, Fd}.
