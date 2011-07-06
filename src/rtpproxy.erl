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
-record(thread, {pid=null, callid=null}).
-record(state, {calls=[], players=[]}).

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

	lists:map(fun(N) ->
			?INFO("Adding node ~p", [N]),
			rtpproxy_ctl:upgrade(N)
		end,
		pool:get_nodes()
	),

	{ok, #state{}}.

handle_call(_Message, _From , State) ->
	{reply, ?RTPPROXY_ERR_SOFTWARE, State}.

handle_cast({message, #cmd{type = ?CMD_V, origin = #origin{pid = Pid}} = Cmd}, State) ->
	% Request basic supported rtpproxy protocol version
	% see available versions here:
	% http://cvs.berlios.de/cgi-bin/viewcvs.cgi/ser/rtpproxy/rtpp_command.c?view=markup
	% We, curently, providing only basic functionality
	gen_server:cast(Pid, {reply, Cmd, "20040107"}),
	{noreply, State};

handle_cast({message, #cmd{type = ?CMD_VF, origin = #origin{pid = Pid}} = Cmd}, State) ->
	% Request additional rtpproxy protocol extensions
	% TODO we should check version capabilities here
	gen_server:cast(Pid, {reply, Cmd, "1"}),
	{noreply, State};

handle_cast({message, #cmd{type = ?CMD_X, origin = #origin{pid = Pid}} = Cmd}, State) ->
	% stop all active sessions
	lists:foreach(fun(X) -> gen_server:cast(X#thread.pid, message_d) end, State#state.calls),
	lists:foreach(fun(X) -> gen_server:cast(X#thread.pid, message_d) end, State#state.players),
	gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_OK}),
	{noreply, State};

handle_cast({message, #cmd{type = ?CMD_I, origin = #origin{pid = Pid}} = Cmd}, State) ->
	% TODO show information about calls
	Stats = lists:map(fun(X) -> gen_server:call(X#thread.pid, message_i) end, State#state.calls),
	% "sessions created: %llu\nactive sessions: %d\n active streams: %d\n"
	% foreach session "%s/%s: caller = %s:%d/%s, callee = %s:%d/%s, stats = %lu/%lu/%lu/%lu, ttl = %d/%d\n"
	gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_OK}),
	{noreply, State};

handle_cast({message, #cmd{origin = #origin{pid = Pid}} = Cmd}, State)  when Cmd#cmd.type == ?CMD_D; Cmd#cmd.type == ?CMD_S ->
	[CallPid, PlayerPid] = lists:map(fun(List) ->
						case lists:keysearch(Cmd#cmd.callid, #thread.callid, List) of
							{value, CI} -> CI#thread.pid;
							NotFound -> false
						end
					end, [State#state.calls, State#state.players]),

	gen_server:cast(CallPid, Cmd#cmd.type),
	try PlayerPid ! Cmd#cmd.type catch E:C -> ok end,

	case (CallPid == false) and (PlayerPid == false) of
		false ->
			gen_server:cast((Cmd#cmd.origin)#origin.pid, {reply, Cmd, ?RTPPROXY_OK});
		true ->
			?ERR("CANNOT find thread(s) to stop.", []),
			gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_OK})
%			gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_ERR_NOSESSION})
	end,
	{noreply, State};

handle_cast({message, #cmd{type = ?CMD_P, origin = #origin{pid = Pid}} = Cmd}, State) ->
	case
		case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.players) of
			{value, PlayerInfo} ->
				% Already started player
				% TODO should we change played music?
				% we simply kill current thread
				PlayerInfo#thread.pid ! ?CMD_S,
				?RTPPROXY_OK;
			NotFound ->
				case
					case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.calls) of
						{value, CallInfo} ->
							case gen_server:call(CallInfo#thread.pid, {Cmd#cmd.type, Cmd#cmd.to}) of
								{ok, A} -> A;
								{error, not_found} -> {error, not_found}
							end;
						_ ->
							% we must fix OpenSER-rtpproxy protocol to add ip:port of destination
							% see 'patches' directory for example
							% TODO probably we need to handle this situation separately
							% TODO contact sobomax for suggestions
							{Ip, Port} = Cmd#cmd.addr,
							{null, Ip, Port}
					end
				of
					{error, notfound} ->
						?RTPPROXY_ERR_NOSESSION;
					Addr ->
						PlayerPid = pool:pspawn(player, start, [Cmd#cmd.filename, Cmd#cmd.codecs, Addr]),
						NewPlayerThread = #thread{pid=PlayerPid, callid=Cmd#cmd.callid},
						{?RTPPROXY_OK, State#state{
							players=lists:append (State#state.players, [NewPlayerThread])
							}
						}
				end
		end
	of
		{Reply, NewState} ->
			gen_server:cast(Pid, {reply, Cmd, Reply}),
			{noreply, NewState};
		Reply ->
			gen_server:cast(Pid, {reply, Cmd, Reply}),
			{noreply, State}
	end;

handle_cast({message, #cmd{type = ?CMD_Q, origin = #origin{pid = Pid}} = Cmd}, State) ->
	case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.calls) of
		{value, CallInfo} ->
			% TODO
			% sprintf(buf, "%s %d %lu %lu %lu %lu\n", cookie, spa->ttl, spa->pcount[idx], spa->pcount[NOT(idx)], spa->pcount[2], spa->pcount[3]);
			gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_OK});
		NotFound ->
			?WARN("Session not exists. Do nothing.", []),
			gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_ERR_NOSESSION})
	end,
	{noreply, State};

handle_cast({message, #cmd{type = ?CMD_C, origin = #origin{pid = Pid}} = Cmd}, State) ->
	case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.calls) of
		{value, CallInfo} ->
			% TODO
			gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_OK});
		NotFound ->
			?WARN("Session not exists. Do nothing.", []),
			gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_ERR_NOSESSION})
	end,
	{noreply, State};

handle_cast({message, #cmd{type = ?CMD_R, origin = #origin{pid = Pid}} = Cmd}, State) ->
	case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.calls) of
		{value, CallInfo} ->
			gen_server:cast(CallInfo#thread.pid, {Cmd#cmd.type, Cmd#cmd.filename}),
			gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_OK});
		NotFound ->
			?WARN("Session not exists. Do nothing.", []),
			gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_ERR_NOSESSION})
	end,
	{noreply, State};

handle_cast({message, #cmd{type = ?CMD_L, origin = #origin{pid = Pid}} = Cmd}, State) ->
	case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.calls) of
		{value, CallInfo} ->
			try gen_server:call(CallInfo#thread.pid, {Cmd#cmd.type, {Cmd#cmd.addr, Cmd#cmd.from, Cmd#cmd.to, Cmd#cmd.params}}) of
				{ok, Reply} ->
					gen_server:cast(Pid, {reply, Cmd, Reply});
				{error, not_found} ->
					?ERR("error not found while CMD_L!", []),
					gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_ERR_NOSESSION});
				{error, udp_error} ->
					?ERR("error in udp while CMD_L!", []),
					gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_ERR_SOFTWARE})
			catch
				Error:ErrorClass ->
					?ERR("Exception {~p,~p} while CMD_L!", [Error,ErrorClass]),
					gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_ERR_SOFTWARE})
			end;
		NotFound ->
			?WARN("Session not exists. Do nothing.", []),
			gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_ERR_NOSESSION})
	end,
	{noreply, State};

handle_cast({message, #cmd{type = ?CMD_U, origin = #origin{pid = Pid}} = Cmd}, State) ->
%	?INFO("Cmd[~p]", [Cmd]),
	{CallPid, NewState} = case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.calls) of
		% Already created session
		{value, CallInfo} ->
			{CallInfo#thread.pid, State};
		NoSessionFound ->
			?INFO("Session not exists. Creating new.", []),
			CPid = pool:pspawn(call, start, [Cmd#cmd.callid]),
			{CPid, State#state{ calls=lists:append (state#state.calls, [#thread{pid=CPid, callid=Cmd#cmd.callid}])}}
	end,

	R = try gen_server:call(CallPid, {Cmd#cmd.type, {Cmd#cmd.addr, Cmd#cmd.from, Cmd#cmd.to, Cmd#cmd.params}}) of
		{ok, new, Reply} -> Reply;
		{ok, old, Reply} -> Reply;
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
	{noreply, NewState};

handle_cast({message, #cmd{origin = #origin{pid = Pid}} = Cmd}, State) ->
	% Unknown command
	gen_server:cast(Pid, {reply, Cmd, ?RTPPROXY_ERR_SOFTWARE}),
	{noreply, State};

handle_cast({call_terminated, {Pid, {ports, Ports}, Reason}}, State) when is_list(Ports) ->
	?INFO("received call [~w] closing due to [~w] returned ports: ~w", [Pid, Reason, Ports]),
	case lists:keytake(Pid, #thread.pid, State#state.calls) of
		{value, CallThread, OtherCalls} ->
			?INFO("call [~w] closed", [Pid]),
			{noreply, State#state{calls=OtherCalls}};
		false ->
			case lists:keytake(Pid, #thread.pid, State#state.players) of
				{value, PlayerThread, OtherPlayers} ->
					?INFO("call [~w] closed", [Pid]),
					{noreply, State#state{players=OtherPlayers}};
				false ->
					?WARN("Cannot find Call or Player with such pid", []),
					{noreply, State}
			end
	end;

handle_cast({node_add, Node}, State) when is_atom(Node) ->
	?INFO("add node [~w]", [Node]),
	pspawn:attach(Node),
	rtpproxy_ctl:upgrade(Node),
	{noreply, State};

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

handle_cast(close_all, State) ->
	% stop all active sessions
	lists:foreach(fun(X) -> gen_server:cast(X#thread.pid, message_d) end, State#state.calls),
	lists:foreach(fun(X) -> gen_server:cast(X#thread.pid, message_d) end, State#state.players),
	{noreply, State};

handle_cast(upgrade, State) ->
	?WARN("UPGRADING beam-files on every node", []),
	lists:foreach(fun(N) ->
			?WARN("UPGRADING on node ~p", [N]),
			rtpproxy_ctl:upgrade(N)
			end, pool:get_nodes()),
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
	end;

handle_info(Info, State) ->
	case Info of
		{udp, Fd, Ip, Port, Data} ->
			{ok, {LocalIp, LocalPort}} = inet:sockname(Fd),
			?WARN("got udp INFO (shouldn't happend) from [~w ~w] to [~w, ~w, ~w] <<some data>> of size ~w", [Ip, Port, Fd, LocalIp, LocalPort, size(Data)]);
		_ ->
			?WARN("got INFO [~w]", [Info])
	end,
	{noreply, State}.

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
