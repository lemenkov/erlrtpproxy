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
	{ok,[[ConfigPath]]} = init:get_argument(config),

	%% Start our pool
	Nodes = [node()|pool:start(rtpproxy, " -config " ++ ConfigPath ++ " ")],

	%% Replace logger with erlsyslog
	rpc:multicall(Nodes, error_logger, add_report_handler, [erlsyslog]),

	%% Run gproc on each node
	rpc:multicall(Nodes, application, set_env, [gproc, gproc_dist, all]),
	rpc:multicall(Nodes, application, start, [gproc]),

	%% Load necessary config files
	rpc:multicall(Nodes, application, load, [rtpproxy]),

	%% Load main module
	application:start(rtpproxy).

stop() ->
	Node = case init:get_plain_arguments() of
		[NodeStr] ->
			list_to_atom(NodeStr);
		_ ->
			halt(1)
	end,
	call(Node, rtpproxy_ctl, command, [#cmd{type = ?CMD_X}], 2),
	call(Node, init, stop, [], 2),
	halt(0).

status() ->
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

% Simply stop all active sessions
command(#cmd{type = ?CMD_X}) ->
	Calls = gproc:lookup_global_properties(media),
	lists:foreach(fun({Pid,{_,_,_,_,_,_}}) -> gen_server:cast(Pid, stop) end, Calls);

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

	case gproc:select([{{{p,g,media},'$1',{CallId,MID,'_','_','_','_'}}, [], ['$1']}]) of
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
