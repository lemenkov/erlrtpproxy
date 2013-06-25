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
-export([save_config/1]).
-export([command/1]).

-include("common.hrl").

start() ->
	%% Replace logger with erlsyslog and remove default tty handler
	error_logger:tty(false),
	error_logger:add_report_handler(erlsyslog),

	%% Load necessary config files
	application:load(rtpproxy),

	%% Load main module
	application:start(rtpproxy).

stop() ->
	%% Stop erlrtpproxy gracefully
	rtpproxy_ctl:command(#cmd{type = ?CMD_X, timestamp = os:timestamp()}),
	%% Stop main node
	init:stop().

save_config(ConfigPath) ->
	{ok,[Config]} = file:consult(ConfigPath),
	NewConfig = proplists:delete(rtpproxy, Config) ++ [{rtpproxy, application:get_all_env(rtpproxy)}],
	file:write_file(ConfigPath, io_lib:format("~p.~n", [NewConfig])).

% Simply stop all active sessions
command(#cmd{type = ?CMD_X}) ->
	[gen_tracker:terminate_child(streams, Name) || {Name, _}  <- gen_tracker:list(streams)],
	ok;

command(#cmd{type = ?CMD_D, callid = C}) ->
	Names = [Name || {Name = {CID,_}, _}  <- gen_tracker:list(streams), CID == C],
	case Names of
		[] -> {error, notfound};
		_ -> [gen_tracker:terminate_child(streams, Name) || Name <- Names], ok
	end;

% DEPRECATED. Use HTTP-JSON.
command(#cmd{type = ?CMD_I}) ->
	{ok, {stats, length(gen_tracker:list(streams))}};

command(#cmd{type = ?CMD_U, callid = C, mediaid = M, from = #party{tag = T, addr = {{0,0,0,0}, _}}, origin = #origin{pid = Pid}} = Cmd) ->
	% Old music-on-hold - FIXME - should start CMD_P actually
	{ok, Role} = gen_tracker:getattr(streams, {C, M}, T),
	{ok, Port} = gen_tracker:getattr(streams, {C, M}, Role),
	backend_ser:reply(Cmd, {{{0,0,0,0}, Port}, {{0,0,0,0}, Port+1}}),
	{ok, sent};

command(#cmd{type = ?CMD_U, callid = C, mediaid = M, from = #party{tag = T}, params = Params, origin = #origin{pid = Pid}} = Cmd) ->
	% Determine IP...
	Ip = case {proplists:get_value(local, Params), proplists:get_value(remote, Params), proplists:get_value(ipv6, Params)} of
		{_, _, true} ->
			{ok, I} = application:get_env(rtpproxy, ipv6), I;
		{undefined, _, _} ->
			{ok, I} = application:get_env(rtpproxy, external), I;
		{{_,_,_,_}, undefined, _} ->
			{ok, I} = application:get_env(rtpproxy, internal), I
	end,

	case gen_tracker:find(streams, {C, M}) of
		undefined ->
			{ok, SP} = gen_tracker:find_or_open(streams, {{C, M}, {supervisor, start_link, [rtpproxy_sup, {C, M, T}]}, temporary, 5000, supervisor, [rtpproxy_sup]}),
			% Start RTP handler
			spawn(
				fun() ->
					% Determine options...
					{ok, TimeoutEarly} = application:get_env(rtpproxy, ttl_early),
					{ok, Timeout} = application:get_env(rtpproxy, ttl),
					{ok, SendRecvStrategy} = application:get_env(rtpproxy, sendrecv),
					{ok, ActiveStrategy} = application:get_env(rtpproxy, active),
					{ok, P1} = gen_tracker:getattr(streams, {C,M}, caller),
					{ok, P2} = gen_tracker:getattr(streams, {C,M}, callee),
					Params1 = Params ++ [{port, P1}, {ip, Ip}, {timeout_early, TimeoutEarly*1000}, {timeout, Timeout*1000}, {sendrecv, SendRecvStrategy}, {active, ActiveStrategy}],
					Params2 = Params ++ [{port, P2}, {ip, Ip}, {timeout_early, TimeoutEarly*1000}, {timeout, Timeout*1000}, {sendrecv, SendRecvStrategy}, {active, ActiveStrategy}],

					% ..and start RTP socket modules
					{ok, RtpPid1} = supervisor:start_child(SP,
						{{phy, C, M, caller}, {gen_server, start_link, [gen_rtp_channel, [Params1], []]}, permanent, 5000, worker, [gen_rtp_channel]}
					),
					error_logger:error_msg("ADDED caller: ~p (~p:~p:~p)~n", [RtpPid1, C, M, T]),
					{ok, RtpPid2} = supervisor:start_child(SP,
						{{phy, C, M, callee}, {gen_server, start_link, [gen_rtp_channel, [Params2], []]}, permanent, 5000, worker, [gen_rtp_channel]}
					),
					error_logger:error_msg("ADDED callee: ~p (~p:~p:~p)~n", [RtpPid1, C, M, T]),

%					gen_server:cast(RtpPid, {update, Params ++ [{sendrecv, SendRecvStrategy}]}),
%					gen_server:cast(RtpPid, {update, [{sendrecv, SendRecvStrategy}, {prefill, {Ip, Addr}}]}),

					gen_server:call(RtpPid1, {rtp_subscriber, {set, RtpPid2}}),
					gen_server:call(RtpPid2, {rtp_subscriber, {set, RtpPid1}}),

					% Check if we need to start recording
					proplists:get_value(copy, Params, false) andalso start_recorder(SP, C, M, T),

					% Check and load (if configured) notification backends
					case application:get_env(rtpproxy, radacct_servers) of
						{ok, _} -> supervisor:start_child(
								SP,
								{{notify_radius, C, M}, {gen_server, start_link, [rtpproxy_notifier_backend_radius, [C, M], []]}, temporary, 5000, worker, [rtpproxy_notifier_backend_radius]}
							);
						_ -> ok
					end,
					case application:get_env(rtpproxy, notify_servers) of
						{ok, NotifyType} ->
							NotifyInfo = proplists:get_value(notify, Params, []),
							((NotifyInfo == []) and (NotifyType == tcp)) orelse supervisor:start_child(
								SP,
								{{notify_openser, C, M}, {gen_server, start_link, [rtpproxy_notifier_backend_notify, [NotifyInfo], []]}, temporary, 5000, worker, [rtpproxy_notifier_backend_notify]}
							);
						_ -> ok
					end
				end
			),
			ok;
		{ok, _} ->
			ok
	end,

	Port = case gen_tracker:getattr(streams, {C, M}, T) of
		{ok, Role} ->
			{ok, P} = gen_tracker:getattr(streams, {C, M}, Role),
			P;
		undefined ->
			{ok, P} = gen_tracker:getattr(streams, {C,M}, callee),
			gen_tracker:setattr(streams, {C,M}, [{T, callee}]),
			P
	end,

	spawn(backend_ser, reply, [Cmd, {{Ip, Port}, {Ip, Port+1}}]),

	{ok, sent};

command(#cmd{type = ?CMD_P, callid = C, mediaid = M, to = #party{tag = T}, params = Params}) ->
	{ok, SupervisorPid} = gen_tracker:find(streams, {C, M}),
	RtpPid1 = gen_tracker:find(streams, {C, M, T}),
	RtpPid2 = gen_tracker:find(streams, {C, M, {other,T}}),
	Ret = supervisor:start_child(SupervisorPid,
		% FIXME we should ignore payload type sent by OpenSIPS/B2BUA and append one currently in use
		{{player, C, M, T}, {gen_server, start_link, [player, [RtpPid1, proplists:get_value(codecs, Params, {'PCMU',8000,1}), binary_to_list(proplists:get_value(filename, Params, <<"default">>)), proplists:get_value(playcount, Params, 0)], []]}, temporary, 5000, worker, [player]}
	),
	gen_server:cast(RtpPid1, {keepalive, disable}),
	gen_server:cast(RtpPid2, {keepalive, disable}),

	gen_server:call(RtpPid1, {rtp_subscriber, {set, null}}),
	gen_server:call(RtpPid2, {rtp_subscriber, {set, null}}),
	ok;

command(#cmd{type = ?CMD_S, callid = C, mediaid = M, to = #party{tag = T}}) ->
	{ok, SupervisorPid} = gen_tracker:find(streams, {C, M}),
	RtpPid1 = gen_tracker:find(streams, {C, M, T}),
	RtpPid2 = gen_tracker:find(streams, {C, M, {other,T}}),
	gen_server:cast(RtpPid1, {keepalive, enable}),
	gen_server:cast(RtpPid2, {keepalive, enable}),

	gen_server:call(RtpPid1, {rtp_subscriber, {set, RtpPid2}}),
	gen_server:call(RtpPid2, {rtp_subscriber, {set, RtpPid1}}),
	supervisor:terminate_child(SupervisorPid, {player, C, M, T}),
	ok;

command(#cmd{type = ?CMD_R, callid = C}) ->
	SupervisorPids = [SID || {{CID,_}, [{pid,SID}|_]}  <- gen_tracker:list(streams), CID == C],
	[ start_recorder(SupervisorPid,CID,MID,TID) || SupervisorPid <- SupervisorPids, {{media, CID, MID, TID}, _, _, _} <- supervisor:which_children(SupervisorPid)],
	ok;

command(_) ->
	% FIXME CMD_C and CMD_Q not yet supported
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions %%
%%%%%%%%%%%%%%%%%%%%%%%%

start_recorder(SupervisorPid, C, M, T) ->
	Ret = supervisor:start_child(SupervisorPid,
			{{recorder, C, M, T}, {gen_server, start_link, [file_writer, [C, M, T], []]}, temporary, 5000, worker, [file_writer]}
		),
	RecorderPid = get_pid(Ret),
	RtpPid = gen_tracker:find(streams, {C, M, T}),
	gen_server:call(RtpPid, {rtp_subscriber, {set, RecorderPid}}),
	ok.

get_pid({ok, Pid}) -> Pid;
get_pid({ok, Pid, _}) -> Pid;
get_pid({error, {already_started, Pid}}) -> Pid.
