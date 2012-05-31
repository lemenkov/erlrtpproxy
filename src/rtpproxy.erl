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

-module(rtpproxy).
-author('lemenkov@gmail.com').

-export([status/0]).
-export([command/1]).

-include("../include/common.hrl").

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
	lists:foreach(fun(Pid) -> gen_server:cast(Pid, stop) end, ets:match(gproc, {{'$1', {'_', '_', {id, '_', '_'}}},'_'}));

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
			?INFO("Media stream does not exist. Creating new.", []),
			pool:pspawn(media, start, [Cmd]);
		[] ->
			?WARN("Media stream does not exist. Do nothing.", []),
			{error, notfound};
		MediaThreads when is_list(MediaThreads) ->
			lists:foreach(fun(Pid) -> gen_server:cast(Pid, Cmd) end, MediaThreads);
		MediaThread ->
			% Operate on existing media thread
			gen_server:cast(MediaThread, Cmd)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions %%
%%%%%%%%%%%%%%%%%%%%%%%%

get_media(CallId, 0) ->
	ets:match(gproc, {{'$1', {'_', '_', {id, CallId, '_'}}},'_'});
get_media(CallId, MediaId) ->
	ets:match(gproc, {{'$1', {'_', '_', {id, CallId, MediaId}}},'_'}).
