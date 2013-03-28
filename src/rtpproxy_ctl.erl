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

command(#cmd{type = ?CMD_D, callid = C}) ->
	SIDs = [SID || {SID = {media_channel_sup, CID, _},_,_,_} <- supervisor:which_children(media_sup), CID == C],
	case SIDs of [] -> {error, notfound};
		_ -> lists:foreach(fun(X) -> supervisor:terminate_child(media_sup, X) end, SIDs), ok
	end;

% DEPRECATED. Use HTTP-JSON.
command(#cmd{type = ?CMD_I}) ->
	{ok, {stats, length(supervisor:which_children(media_sup))}};

command(#cmd{type = ?CMD_U, callid = CallId, mediaid = MediaId, from = #party{tag = Tag}} = Cmd) ->
	case gproc:where({n,l,{media, CallId, MediaId, Tag}}) of
		undefined ->
			start_media(Cmd);
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

%%
%% Private functions
%%

start_media(#cmd{callid = C, mediaid = M, from = #party{tag = T}, params = Params} = Cmd) ->
	Ret0 = supervisor:start_child(media_sup,
		{
			{media_channel_sup, C, M},
			{supervisor, start_link, [rtpproxy_sup, media_channel_sup]},
			temporary,
			5000,
			supervisor,
			[rtpproxy_sup]
		}
	),

	% Start main media module
	Ret1 = supervisor:start_child(get_pid(Ret0),
		{{media, C, M, T}, {gen_server, start_link, [media, [Cmd], []]}, permanent, 5000, worker, [media]}
	),

	% Determine options...
	Ip = case {proplists:get_value(local, Params), proplists:get_value(remote, Params), proplists:get_value(ipv6, Params)} of
		{_, _, true} ->
			{ok, I} = application:get_env(rtpproxy, ipv6), I;
		{undefined, _, _} ->
			{ok, I} = application:get_env(rtpproxy, external), I;
		{{_,_,_,_}, undefined, _} ->
			{ok, I} = application:get_env(rtpproxy, internal), I
	end,

	{ok, RebuildRtp} = application:get_env(rtpproxy, rebuildrtp),
	{ok, TimeoutEarly} = application:get_env(rtpproxy, ttl_early),
	{ok, Timeout} = application:get_env(rtpproxy, ttl),
	{ok, SendRecvStrategy} = application:get_env(rtpproxy, sendrecv),
	{ok, ActiveStrategy} = application:get_env(rtpproxy, active),
	Params1 = Params ++ [{parent, get_pid(Ret1)}, {port, 0}, {ip, Ip}, {rebuildrtp, RebuildRtp}, {timeout_early, TimeoutEarly*1000}, {timeout, Timeout*1000}, {sendrecv, SendRecvStrategy}, {active, ActiveStrategy}],

	% ..and start RTP socket module
	{ok, RtpPid0} = supervisor:start_child(get_pid(Ret0),
		{{phy, C, M, T}, {gen_server, start_link, [gen_rtp_channel, [Params1], []]}, permanent, 5000, worker, [gen_rtp_channel]}
	),

	case Ret0 of
		{error, _} ->
			% That's a 2nd side

			% Set RTP path
			[RtpPid1 | _ ] = [P || {{phy, C0, M0, T0}, P, _, _} <- supervisor:which_children(get_pid(Ret0)), T0 /= T, C0 == C, M0 == M],
			gen_server:call(RtpPid0, {rtp_subscriber, RtpPid1}),
			gen_server:call(RtpPid1, {rtp_subscriber, RtpPid0}),
			ok;
		_ ->
			ok
	end.

get_pid({ok, Pid}) -> Pid;
get_pid({ok, Pid, _}) -> Pid;
get_pid({error, {already_started, Pid}}) -> Pid.
