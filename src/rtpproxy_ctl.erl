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

-module(rtpproxy_ctl).
-author('lemenkov@gmail.com').

-export([start/0]).
-export([stop/0]).
-export([status_pp/0]).
-export([status/0]).
-export([command/1]).

-include("../include/common.hrl").

start() ->
	% Start our pool
	{ok,[[ConfigPath]]} = init:get_argument(config),
	Nodes = [node()|pool:start(rtpproxy, " -config " ++ ConfigPath ++ " ")],

	% Replace logger with erlsyslog
	rpc:multicall(Nodes, error_logger, add_report_handler, erlsyslog),

	% Run gproc on each node
	rpc:multicall(Nodes, application, set_env, [gproc, gproc_dist, all]),
	rpc:multicall(Nodes, application, start, gproc),

	% Load necessary config files
	rpc:multicall(Nodes, application, load, rtpproxy),

	% Load main module
	application:start(rtpproxy).

stop() ->
	Status = case init:get_plain_arguments() of
		[NodeStr] ->
			Node = list_to_atom(NodeStr),
			try rpc:call(Node, rtpproxy_ctl, command, [#cmd{type = ?CMD_X}], 5000) of
				{badrpc, _} ->
					2;
				_ ->
					case rpc:call(Node, init, stop, [], 5000) of
						{badrpc, _} ->
							2;
						_ ->
							0
					end
			catch _:_ ->
				2
			end;
		_ ->
			1
	end,
	halt(Status).

status_pp() ->
	Status = case init:get_plain_arguments() of
		[NodeStr] ->
			Node = list_to_atom(NodeStr),
			try rpc:call(Node, rtpproxy_ctl, command, [#cmd{type = ?CMD_I, params = [brief]}], 5000) of
%			try rpc:call(Node, rtpproxy_ctl, status, [], 5000) of
				{badrpc, _} ->
					4;
				{ok, {stats, Number}} ->
					io:format("active calls: ~p~n", [Number]),
					0;
				undefined ->
					ok = io:format("~n"),
					3
			catch _:_ ->
				ok = io:format("~n"),
				4
			end;
		_ ->
			ok = io:format("~n"),
			4
	end,
	halt(Status).

% FIXME this must be reworked (no preformatted strings - just plain stats)
status() ->
	Calls = ets:match(gproc, {{'$1', {'_', '_', {id, '$2', '$3'}}},'_'}),
	Header = io_lib:format("Current state - ~p media stream(s):~n", [length(Calls)]),
	?INFO(Header, []),
	MediaInfos = lists:map(fun({Pid, CallId, MediaId}) ->
			{ok, Reply} = gen_server:call(Pid, ?CMD_Q),
			MediaInfo = io_lib:format("* CallID: ~s, MediaId: ~p, ~s~n", [CallId, MediaId, Reply]),
			?INFO(MediaInfo, []),
			MediaInfo
		end,
	Calls),
	lists:flatten([Header] ++ MediaInfos).

% Simply stop all active sessions
command(#cmd{type = ?CMD_X}) ->
	Pids = case ets:match(gproc, {{'$1', {'_', '_', {id, '_', '_'}}},'_'}) of
		[] -> [];
		Ret -> [ X || [X] <- Ret ]
	end,
	lists:foreach(fun(Pid) -> gen_server:cast(Pid, stop) end, Pids);

command(#cmd{type = ?CMD_I, params = [brief]}) ->
	Length = length(ets:match(gproc, {{'$1', {'_', '_', {id, '_', '_'}}},'_'})),
	{ok, {stats, Length}};

% TODO show information about calls
command(#cmd{type = ?CMD_I}) ->
	% Calls = ets:match(gproc, {{'$1', {'_', '_', {id, '_', '_'}}},'_'}),
	% Stats = lists:map(fun(Pid) -> gen_server:call(Pid, ?CMD_Q) end, Calls),
	% "sessions created: %llu\nactive sessions: %d\n active streams: %d\n"
	% foreach session "%s/%s: caller = %s:%d/%s, callee = %s:%d/%s, stats = %lu/%lu/%lu/%lu, ttl = %d/%d\n"
	Length = length(ets:match(gproc, {{'$1', {'_', '_', {id, '_', '_'}}},'_'})),
	{ok, {stats, Length}};

command(#cmd{callid = CallId, mediaid = MediaId} = Cmd) ->
	% First try to find existing session(s)
	case get_media(CallId, MediaId) of
		[] when Cmd#cmd.type == ?CMD_U ->
			% Create new media thread
			error_logger:warning_msg("Media stream does not exist. Creating new."),
			pool:pspawn(media, start, [Cmd]),
			{ok, sent};
		[] ->
			error_logger:warning_msg("Media stream does not exist. Do nothing."),
			{error, notfound};
		MediaThread when is_pid(MediaThread) ->
			% Operate on existing media thread - wait for answer
			gen_server:cast(MediaThread, Cmd),
			{ok, sent};
		MediaThreads when is_list(MediaThreads) ->
			% Group command - return immediately
			lists:foreach(fun(Pid) -> gen_server:cast(Pid, Cmd) end, MediaThreads)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions %%
%%%%%%%%%%%%%%%%%%%%%%%%

get_media(CallId, 0) ->
	case ets:match(gproc, {{'$1', {'_', '_', {id, CallId, '_'}}},'_'}) of
		[] -> [];
		Ret -> [ X || [X] <- Ret ]
	end;
get_media(CallId, MediaId) ->
	case ets:match(gproc, {{'$1', {'_', '_', {id, CallId, MediaId}}},'_'}) of
		[[Pid]] -> Pid;
		[] -> []
	end.
