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

-export([acc/4]).
-export([start/0]).
-export([stop/0]).
-export([stop_sysv/0]).
-export([status_sysv/0]).
-export([status/0]).
-export([command/1]).

-include("../include/common.hrl").

acc(Type, CallId, MediaId, Addr) when Type == start; Type == interim_update; Type == stop ->
	{ok, IgnoreStart} = application:get_env(rtpproxy, ignore_start),
	{ok, IgnoreStop} = application:get_env(rtpproxy, ignore_stop),
	Send = ((Type == start) and not IgnoreStart) or (Type == interim_update) or ((Type == stop) and not IgnoreStop),
	Send andalso gen_server:cast(rtpproxy_notifier_backend_radius, {Type, CallId, MediaId, Addr}),
	Send andalso gen_server:cast(rtpproxy_notifier_backend_notify, {Type, CallId, MediaId, Addr}),
	ok.

start() ->
	% Start our pool
	ConfigPath = case init:get_argument(config) of
		error -> "../priv/erlrtpproxy.config"; % for testing
		{ok,[[CPath]]} -> CPath
	end,

	Nodes = [node()|pool:start(rtpproxy, " -config " ++ ConfigPath ++ " ")],

	% Replace logger with erlsyslog
	rpc:multicall(Nodes, error_logger, add_report_handler, [erlsyslog]),

	% Run gproc on each node
	rpc:multicall(Nodes, application, set_env, [gproc, gproc_dist, all]),
	rpc:multicall(Nodes, application, start, [gproc]),

	% Load necessary config files
	rpc:multicall(Nodes, application, load, [rtpproxy]),

	% Load main module
	application:start(rtpproxy).

stop() ->
	application:stop(rtpproxy),
	gen_server:cast(listener, stop),
	gen_server:cast(rtpproxy_notifier_backend_notify, stop),
	gen_server:cast(rtpproxy_notifier_backend_radius, stop),
	gen_server:cast(backend_ser, stop),
	gen_server:cast(file_writer, stop),
	gen_server:cast(storage, stop),
	application:stop(gproc),
	pool:stop(),
	net_kernel:stop().

stop_sysv() ->
	Node = case init:get_plain_arguments() of
		[NodeStr] ->
			list_to_atom(NodeStr);
		_ ->
			halt(1)
	end,
	call(Node, rtpproxy_ctl, command, [#cmd{type = ?CMD_X}], 2),
	call(Node, init, stop, [], 2),
	halt(0).

status_sysv() ->
	Node = case init:get_plain_arguments() of
		[NodeStr] ->
			list_to_atom(NodeStr);
		_ ->
			halt(1)
	end,
	case call(Node, rtpproxy_ctl, command, [#cmd{type = ?CMD_I, params = [brief]}], 4) of
		{ok, {stats, Number}} ->
			io:format("active calls: ~p~n", [Number]),
			halt(0);
		undefined ->
			ok = io:format("~n"),
			halt(3)
	end.

% FIXME this must be reworked (no preformatted strings - just plain stats)
status() ->
	Calls = gproc:lookup_global_properties(media),
	Header = io_lib:format("Current state - ~p media stream(s):~n", [length(Calls)]),
	?INFO(Header, []),
	MediaInfos = lists:map(fun({Pid,{id,CallId,MediaId}}) ->
			{ok, Reply} = gen_server:call(Pid, ?CMD_Q),
			MediaInfo = io_lib:format("* CallID: ~s, MediaId: ~p, ~s~n", [CallId, MediaId, Reply]),
			?INFO(MediaInfo, []),
			MediaInfo
		end,
	Calls),
	lists:flatten([Header] ++ MediaInfos).

% Simply stop all active sessions
command(#cmd{type = ?CMD_X}) ->
	Pids = gproc:lookup_global_properties(media),
	lists:foreach(fun({Pid,{id,_CallId,_MediaId}}) -> gen_server:cast(Pid, stop) end, Pids);

command(#cmd{type = ?CMD_I, params = [brief]}) ->
	Length = length(gproc:lookup_global_properties(media)) div 2,
	{ok, {stats, Length}};

% TODO show information about calls
command(#cmd{type = ?CMD_I}) ->
	% Calls = gproc:lookup_global_properties(media),
	% Stats = lists:map(fun(Pid) -> gen_server:call(Pid, ?CMD_Q) end, Calls),
	% "sessions created: %llu\nactive sessions: %d\n active streams: %d\n"
	% foreach session "%s/%s: caller = %s:%d/%s, callee = %s:%d/%s, stats = %lu/%lu/%lu/%lu, ttl = %d/%d\n"
	Length = length(gproc:lookup_global_properties(media)) div 2,
	% FIXME - provide real stats here
	{ok, {stats, Length, Length}};

command(#cmd{type = ?CMD_U, callid = CallId, mediaid = MediaId, from = #party{tag = Tag}} = Cmd) ->
	case gproc:lookup_global_name({media, CallId, MediaId, Tag}) of
		undefined ->
			media:start(Cmd);
		MediaThread ->
			gen_server:cast(MediaThread, Cmd)
	end,
	{ok, sent};
command(#cmd{callid = CallId, mediaid = MediaId} = Cmd) ->
	% First try to find existing session(s)
	MID = case MediaId of
		0 -> '_';
		_ -> MediaId
	end,

	case gproc:select([{{{p,g,media},'$1',{id,CallId,MID}}, [], ['$1']}]) of
		[] ->
			error_logger:warning_msg("Media stream does not exist. Do nothing."),
			{error, notfound};
		MediaThreads when is_list(MediaThreads) ->
			% Group command - return immediately
			lists:foreach(fun(Pid) -> gen_server:cast(Pid, Cmd) end, MediaThreads)
	end.

%%
%% Private functions
%%

call(Node, M, F, A, HaltRet) ->
	try rpc:call(Node, M, F, A, 5000) of
		{badrpc, _} ->
			halt(HaltRet);
		Rest ->
			Rest
	catch _:_ ->
		halt(HaltRet)
	end.
