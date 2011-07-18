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

-behaviour(gen_server).
-export([start/0]).
-export([start/1]).
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include("../include/common.hrl").

% description of call thread
-record(thread, {pid=null, callid=null, mediaid=0}).
-record(state, {calls=[]}).

start() ->
	gen_server:start({global, ?MODULE}, ?MODULE, [], []).

start(Args) ->
	gen_server:start({global, ?MODULE}, ?MODULE, Args, []).

start_link(Args) ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, Args, []).

init(_Unused) ->
	process_flag(trap_exit, true),

	% Load parameters
	{ok, {SyslogHost, SyslogPort}} = application:get_env(?MODULE, syslog_address),

	error_logger:add_report_handler(erlsyslog, {0, SyslogHost, SyslogPort}),
	error_logger:tty(false),

	pool:start(rtpproxy),

%	lists:map(fun(N) ->
%			?INFO("Adding node ~p", [N]),
%			rtpproxy_ctl:upgrade(N)
%		end,
%		pool:get_nodes()
%	),
	?INFO("Adding node(s) ~p", [pool:get_nodes()]),

	{ok, #state{}}.

handle_call(_Message, _From , State) ->
	{reply, ?RTPPROXY_ERR_SOFTWARE, State}.

handle_cast({message, #cmd{type = ?CMD_V, origin = #origin{pid = Pid}} = Cmd}, State) ->
	% Request basic supported rtpproxy protocol version
	% see available versions here:
	% http://sippy.git.sourceforge.net/git/gitweb.cgi?p=sippy/rtpproxy;a=blob;f=rtpp_command.c#l58
	% We, curently, provide only basic functionality
	gen_server:cast(Pid, {reply, Cmd, "20040107"}),
	{noreply, State};

handle_cast({message, #cmd{type = ?CMD_VF, origin = #origin{pid = Pid}} = Cmd}, State) ->
	% Request additional rtpproxy protocol extensions
	% TODO we should check version capabilities here
	gen_server:cast(Pid, {reply, Cmd, "1"}),
	{noreply, State};

handle_cast({message, #cmd{type = ?CMD_X, origin = #origin{pid = Pid}} = Cmd}, State) ->
	% stop all active sessions
	lists:foreach(fun(X) -> gen_server:cast(X#thread.pid, stop) end, State#state.calls),
	gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_OK}),
	{noreply, State};

handle_cast({message, #cmd{type = ?CMD_I, origin = #origin{pid = Pid}} = Cmd}, State) ->
	% TODO show information about calls
	Stats = lists:map(fun(X) -> gen_server:call(X#thread.pid, ?CMD_Q) end, State#state.calls),
	% "sessions created: %llu\nactive sessions: %d\n active streams: %d\n"
	% foreach session "%s/%s: caller = %s:%d/%s, callee = %s:%d/%s, stats = %lu/%lu/%lu/%lu, ttl = %d/%d\n"
	gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_OK}),
	{noreply, State};

% First try to find existing session
handle_cast({message, #cmd{origin = #origin{pid = Pid}} = Cmd}, State) ->
	case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.calls) of
		{value, CallInfo} ->
			% Operate on existing media thread
			handle_cast({CallInfo#thread.pid, Cmd}, State);
		false when Cmd#cmd.type == ?CMD_U ->
			% Create new media thread
			handle_cast({false, Cmd}, State);
		false ->
			?WARN("Session not exists. Do nothing.", []),
			gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_ERR_NOSESSION}),
			{noreply, State}
	end;

handle_cast({CallPid, #cmd{origin = #origin{pid = Pid}} = Cmd}, State)  when Cmd#cmd.type == ?CMD_D; Cmd#cmd.type == ?CMD_S ->
	gen_server:cast(CallPid, Cmd#cmd.type),
	gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_OK}),
	{noreply, State};

handle_cast({CallPid, #cmd{type = ?CMD_P, origin = #origin{pid = Pid}} = Cmd}, State) ->
	gen_server:cast(CallPid, Cmd#cmd.type, Cmd#cmd.filename, Cmd#cmd.codecs),
	gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_OK}),
	{noreply, State};

handle_cast({CallPid, #cmd{type = ?CMD_Q, origin = #origin{pid = Pid}} = Cmd}, State) ->
	{ok, Reply} = gen_server:call(CallPid, ?CMD_Q),
	% TODO
	gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_OK}),
	{noreply, State};

handle_cast({CallPid, #cmd{type = ?CMD_C, origin = #origin{pid = Pid}} = Cmd}, State) ->
	% TODO
	gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_OK}),
	{noreply, State};

handle_cast({CallPid, #cmd{type = ?CMD_R, origin = #origin{pid = Pid}} = Cmd}, State) ->
	gen_server:cast(CallPid, {Cmd#cmd.type, Cmd#cmd.filename}),
	gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_OK}),
	{noreply, State};

handle_cast({false, #cmd{type = ?CMD_U, origin = #origin{pid = Pid}, from = {Tag, MediaId}} = Cmd}, State) ->
	% TODO
	?INFO("Session not exists. Creating new.", []),
	pool:pspawn(media, start, [Cmd]),
	{noreply, State};

handle_cast({CallPid, #cmd{type = ?CMD_U, origin = #origin{pid = Pid}, from = {Tag, MediaId}} = Cmd}, State) ->
	R = try gen_server:call(CallPid, {Cmd#cmd.type, Cmd#cmd.addr, Cmd#cmd.from}) of
		{ok, Reply} -> Reply;
		{error, not_found} ->
			?ERR("Session does not exists.", []),
			?RTPPROXY_ERR_NOSESSION;
		{error, udp_error} ->
			?ERR("error in udp while CMD_U!", []),
			?RTPPROXY_ERR_SOFTWARE
	catch
		Error:ErrorClass ->
			?ERR("Exception {~p,~p} while CMD_U!", [Error,ErrorClass]),
			?RTPPROXY_ERR_SOFTWARE
	end,
	gen_server:cast(Pid, {reply, Cmd, R}),
	case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.calls) of
		{value, CallInfo} ->
			{noreply, State};
		false ->
			{noreply, State#state{calls=lists:append(State#state.calls, [#thread{pid=CallPid, callid=Cmd#cmd.callid}])}}
	end;

handle_cast({CallPid, #cmd{type = ?CMD_L, origin = #origin{pid = Pid}} = Cmd}, State) ->
	R = try gen_server:call(CallPid, {Cmd#cmd.type, Cmd#cmd.addr, Cmd#cmd.to}) of
		{ok, Reply} -> Reply;
		{error, not_found} ->
			?ERR("Session does not exists.", []),
			?RTPPROXY_ERR_NOSESSION;
		{error, udp_error} ->
			?ERR("error in udp while CMD_L!", []),
			?RTPPROXY_ERR_SOFTWARE
	catch
		Error:ErrorClass ->
			?ERR("Exception {~p,~p} while CMD_L!", [Error,ErrorClass]),
			?RTPPROXY_ERR_SOFTWARE
	end,
	gen_server:cast(Pid, {reply, Cmd, R}),
	{noreply, State};

handle_cast({_, #cmd{origin = #origin{pid = Pid}} = Cmd}, State) ->
	% Unknown command
	gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_ERR_SOFTWARE}),
	{noreply, State};

handle_cast({call_terminated, Pid, Reason}, State) ->
	?INFO("received call [~w] closing due to [~w]", [Pid, Reason]),
	case lists:keytake(Pid, #thread.pid, State#state.calls) of
		{value, CallThread, OtherCalls} ->
			?INFO("call [~w] closed", [Pid]),
			{noreply, State#state{calls=OtherCalls}};
		false ->
			?WARN("Cannot find Media with such pid", []),
			{noreply, State}
	end;

handle_cast(status, State) ->
	?INFO("Current state - ~p call(s):", [length(State#state.calls)]),
	lists:foreach(	fun(X) ->
				% TODO fix this strange situation
				{ok, Reply} = try gen_server:call(X#thread.pid, ?CMD_I) catch E:C -> {ok, [["died (shouldn't happend)"]]} end,
				?INFO("* Pid: ~p, CallID: ~p, State:", [X#thread.pid, X#thread.callid]),
				lists:foreach(	fun(Y) ->
							lists:foreach(	fun(Z) ->
										?INFO("---> ~s", [Z])
									end,
								Y)
							end,
					Reply)
			end,
		State#state.calls),
	?INFO("Current state: END.", []),
	{noreply, State};

handle_cast(_Other, State) ->
	{noreply, State}.

% Call died (due to timeout)
handle_info({'EXIT', Pid, Reason}, State) ->
	?INFO("received 'EXIT' from ~p, closing due to [~p]", [Pid, Reason]),
	case lists:keysearch(Pid, #thread.pid, State#state.calls) of
		{value, CallThread} ->
			?INFO("call [~w] closed", [Pid]),
			{noreply, State#state{calls=lists:delete(CallThread, State#state.calls)}};
		false ->
			{noreply, State}
	end.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate({ErrorClass, {Module,Function, [Pid, Message]}}, State) ->
	?ERR("RTPPROXY terminated due to Error [~p] in ~p:~p(...) with Msg[~p] from Pid ~p", [ErrorClass, Module, Function, Message, Pid]),
	error_logger:delete_report_handler(erlsyslog),
	error_logger:tty(true);

terminate(Reason, State) ->
	?ERR("RTPPROXY terminated due to reason [~w]", [Reason]),
	error_logger:delete_report_handler(erlsyslog),
	error_logger:tty(true).
