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
-export([save_config/1]).
-export([command/1]).

-include("common.hrl").

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
	rpc:multicall(Nodes, application, start, [gproc]),

	%% Load necessary config files
	rpc:multicall(Nodes, application, load, [rtpproxy]),

	%% Load main module
	application:start(rtpproxy).

stop() ->
	%% Stop erlrtpproxy gracefully
	rtpproxy_ctl:command(#cmd{type = ?CMD_X}),
	%% Stop all slave nodes
	pool:stop(),
	%% Stop main node
	init:stop().

save_config(ConfigPath) ->
	{ok,[Config]} = file:consult(ConfigPath),
	NewConfig = proplists:delete(rtpproxy, Config) ++ [{rtpproxy, application:get_all_env(rtpproxy)}],
	file:write_file(ConfigPath, io_lib:format("~p.~n", [NewConfig])).

% Simply stop all active sessions
command(#cmd{type = ?CMD_X}) ->
	[supervisor:terminate_child(media_sup, SID) || {SID,_,_,_} <- supervisor:which_children(media_sup)],
	ok;

% DEPRECATED. Use HTTP-JSON.
command(#cmd{type = ?CMD_I}) ->
	Length = gproc:get_value({c,l,calls}, shared),
	{ok, {stats, Length}};

command(#cmd{type = ?CMD_U, callid = CallId, mediaid = MediaId, from = #party{tag = Tag}} = Cmd) ->
	case gproc:where({n,l,{media, CallId, MediaId, Tag}}) of
		undefined ->
			rtpproxy_sup:start_media(Cmd);
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

	case gproc:select([{{{n,l,{media, CallId, MID, '_'}},'$1','_'}, [], ['$1']}]) of
		[] ->
			error_logger:warning_msg("Media stream does not exist. Do nothing."),
			{error, notfound};
		MediaThreads when is_list(MediaThreads) ->
			% Group command - return immediately
			lists:foreach(fun(Pid) -> gen_server:cast(Pid, Cmd) end, MediaThreads)
	end.
