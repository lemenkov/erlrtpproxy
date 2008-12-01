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
-export([start/1]).
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

% include list of hosts
-include("config.hrl").
-include("common.hrl").

% description of call thread
-record(thread, {pid=null, callid=null, node=null}).
-record(state, {calls=[], rtphosts=null, players=[]}).

start(Args) ->
	gen_server:start({global, ?MODULE}, ?MODULE, Args, []).

start_link(Args) ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, Args, []).

init(_Unused) ->
	process_flag(trap_exit, true),
	error_logger:tty(false)
	syslog:start(),
	RH = lists:map(
		fun({Node, Ip, {min_port, MinPort}, {max_port, MaxPort}}) ->
			case net_adm:ping(Node) of
				pong ->
					?INFO("Adding node ~p at ip ~p with port range [~p - ~p]", [Node, Ip, MinPort, MaxPort]),
					{Node, Ip, lists:seq(MinPort, MaxPort, ?PORTS_PER_MEDIA)};
				pang ->
					?ERR("Failed to add node ~p at ip ~p with port range [~p - ~p]", [Node, Ip, MinPort, MaxPort]),
					[]
			end
		end,
		?RtpHosts),
	{ok, #state{rtphosts=RH}}.

handle_call(_Message, _From , State) ->
	{reply, ?RTPPROXY_ERR_SOFTWARE, State}.

handle_cast({message, Cmd}, State) when Cmd#cmd.type == ?CMD_V ->
	% Request basic supported rtpproxy protocol version
%	{ "20040107", "Basic RTP proxy functionality" },
%	{ "20050322", "Support for multiple RTP streams and MOH" },
%	{ "20060704", "Support for extra parameter in the V command" },
%	{ "20071116", "Support for RTP re-packetization" },
%	{ "20071218", "Support for forking (copying) RTP stream" },
%	{ "20080403", "Support for RTP statistics querying" },
%	{ "20081102", "Support for setting codecs in the update/lookup commend" },
	gen_server:cast((Cmd#cmd.origin)#origin.pid, {reply, Cmd, "20040107"}),
	{noreply, State};

handle_cast({message, Cmd}, State) when Cmd#cmd.type == ?CMD_VF ->
	% Request additional rtpproxy protocol extensions
	% TODO we should check version capabilities here
	gen_server:cast((Cmd#cmd.origin)#origin.pid, {reply, Cmd, "1"}),
	{noreply, State};

handle_cast({message, Cmd}, State) when Cmd#cmd.type == ?CMD_X ->
	% stop all active sessions
	lists:foreach(fun(X) -> gen_server:cast(X#thread.pid, message_d) end, State#state.calls),
	lists:foreach(fun(X) -> gen_server:cast(X#thread.pid, message_d) end, State#state.players),
	gen_server:cast((Cmd#cmd.origin)#origin.pid, {reply, Cmd, ?RTPPROXY_OK}),
	{noreply, State};

handle_cast({message, Cmd}, State) when Cmd#cmd.type == ?CMD_I ->
	% TODO show information about calls
	Stats = lists:map(fun(X) -> gen_server:call(X#thread.pid, message_i) end, State#state.calls),
	% "sessions created: %llu\nactive sessions: %d\n active streams: %d\n"
	% foreach session "%s/%s: caller = %s:%d/%s, callee = %s:%d/%s, stats = %lu/%lu/%lu/%lu, ttl = %d/%d\n"
	gen_server:cast((Cmd#cmd.origin)#origin.pid, {reply, Cmd, ?RTPPROXY_OK}),
	{noreply, State};

handle_cast({message, Cmd}, State) when Cmd#cmd.type == ?CMD_D; Cmd#cmd.type == ?CMD_S ->
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
			gen_server:cast((Cmd#cmd.origin)#origin.pid, {reply, Cmd, ?RTPPROXY_ERR_NOSESSION})
	end,
	{noreply, State};

handle_cast({message, Cmd}, State) when Cmd#cmd.type == ?CMD_P ->
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
						case find_host(State#state.rtphosts) of
							{RtpHost={Node, NodeIp, AvailablePorts}, RtpHosts} ->
								try rpc:call(Node, player, start, [Cmd#cmd.filename, Cmd#cmd.codecs, Addr]) of
									{ok, PlayerPid} ->
										NewPlayerThread = #thread{pid=PlayerPid, callid=Cmd#cmd.callid, node=Node},
										{?RTPPROXY_OK, State#state{
												players=lists:append (State#state.players, [NewPlayerThread]),
												rtphosts=RtpHosts ++ [RtpHost]
											}
										};
									{badrpc, nodedown} ->
										?ERR("rtp host [~w] seems stopped!", [{Node,NodeIp}]),
										% FIXME consider to remove bad host from list
										{?RTPPROXY_ERR_SOFTWARE,  State#state{rtphosts=RtpHosts++[RtpHost]}};
									Other ->
										?ERR("error creating call! [~w]", [Other]),
										{?RTPPROXY_ERR_SOFTWARE,  State#state{rtphosts=RtpHosts++[RtpHost]}}
								catch
									ExceptionClass:ExceptionPattern ->
										?ERR("Exception [~w] reached with result [~w]", [ExceptionClass, ExceptionPattern]),
										{?RTPPROXY_ERR_SOFTWARE,  State#state{rtphosts=RtpHosts++[RtpHost]}}
								end;
							error_no_nodes ->
								?ERR("error no suitable nodes to create new player!", []),
								?RTPPROXY_ERR_SOFTWARE
						end
				end
		end
	of
		{Reply, NewState} ->
			gen_server:cast((Cmd#cmd.origin)#origin.pid, {reply, Cmd, Reply}),
			{noreply, NewState};
		Reply ->
			gen_server:cast((Cmd#cmd.origin)#origin.pid, {reply, Cmd, Reply}),
			{noreply, State}
	end;

handle_cast({message, Cmd}, State) when Cmd#cmd.type == ?CMD_Q ->
	case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.calls) of
		{value, CallInfo} ->
			% TODO
			% sprintf(buf, "%s %d %lu %lu %lu %lu\n", cookie, spa->ttl, spa->pcount[idx], spa->pcount[NOT(idx)], spa->pcount[2], spa->pcount[3]);
			gen_server:cast((Cmd#cmd.origin)#origin.pid, {reply, Cmd, ?RTPPROXY_OK});
		NotFound ->
			?WARN("Session not exists. Do nothing.", []),
			gen_server:cast((Cmd#cmd.origin)#origin.pid, {reply, Cmd, ?RTPPROXY_ERR_NOSESSION})
	end,
	{noreply, State};

handle_cast({message, Cmd}, State) when Cmd#cmd.type == ?CMD_C ->
	case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.calls) of
		{value, CallInfo} ->
			% TODO
			gen_server:cast((Cmd#cmd.origin)#origin.pid, {reply, Cmd, ?RTPPROXY_OK});
		NotFound ->
			?WARN("Session not exists. Do nothing.", []),
			gen_server:cast((Cmd#cmd.origin)#origin.pid, {reply, Cmd, ?RTPPROXY_ERR_NOSESSION})
	end,
	{noreply, State};

handle_cast({message, Cmd}, State) when Cmd#cmd.type == ?CMD_R ->
	case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.calls) of
		{value, CallInfo} ->
			gen_server:cast(CallInfo#thread.pid, {Cmd#cmd.type, Cmd#cmd.filename}),
			gen_server:cast((Cmd#cmd.origin)#origin.pid, {reply, Cmd, ?RTPPROXY_OK});
		NotFound ->
			?WARN("Session not exists. Do nothing.", []),
			gen_server:cast((Cmd#cmd.origin)#origin.pid, {reply, Cmd, ?RTPPROXY_ERR_NOSESSION})
	end,
	{noreply, State};

handle_cast({message, Cmd}, State) when Cmd#cmd.type == ?CMD_L ->
	case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.calls) of
		{value, CallInfo} ->
			try gen_server:call(CallInfo#thread.pid, {Cmd#cmd.type, {Cmd#cmd.addr, Cmd#cmd.from, Cmd#cmd.to, Cmd#cmd.params}}) of
				{ok, Reply} ->
					gen_server:cast((Cmd#cmd.origin)#origin.pid, {reply, Cmd, Reply});
				{error, not_found} ->
					?ERR("error not found while CMD_L!", []),
					gen_server:cast((Cmd#cmd.origin)#origin.pid, {reply, Cmd, ?RTPPROXY_ERR_NOSESSION});
				{error, udp_error} ->
					?ERR("error in udp while CMD_L!", []),
					gen_server:cast((Cmd#cmd.origin)#origin.pid, {reply, Cmd, ?RTPPROXY_ERR_SOFTWARE})
			catch
				Error:ErrorClass ->
					?ERR("Exception {~p,~p} in udp while CMD_L!", [Error,ErrorClass]),
					gen_server:cast((Cmd#cmd.origin)#origin.pid, {reply, Cmd, ?RTPPROXY_ERR_SOFTWARE})
			end;
		NotFound ->
			?WARN("Session not exists. Do nothing.", []),
			gen_server:cast((Cmd#cmd.origin)#origin.pid, {reply, Cmd, ?RTPPROXY_ERR_NOSESSION})
	end,
	{noreply, State};

handle_cast({message, Cmd}, State) when	Cmd#cmd.type == ?CMD_U ->
%	?INFO("Cmd[~p]", [Cmd]),
	case
		case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.calls) of
			% Already created session
			{value, CallInfo} ->
%				?INFO("Session exists. Update/create existing session.", []),
				case find_host_by_node(CallInfo#thread.node, State#state.rtphosts) of
					false ->
						?RTPPROXY_ERR_SOFTWARE;
					{Node, NodeIp, []} ->
						% cannot start session on that node - no available ports
						?RTPPROXY_ERR_SOFTWARE;
					RtpHost={Node, NodeIp, [NewPort|AvailablePorts]} ->
						case gen_server:call(CallInfo#thread.pid, {Cmd#cmd.type, {NewPort, Cmd#cmd.addr, Cmd#cmd.from, Cmd#cmd.to, Cmd#cmd.params}}) of
							{ok, new, Reply} ->
								{Reply, State#state{rtphosts=lists:delete(RtpHost, State#state.rtphosts) ++ [{Node, NodeIp, AvailablePorts}]}};
							{ok, old, Reply} ->
								Reply;
							{error, udp_error} ->
								?ERR("error in udp while CMD_U!", []),
								?RTPPROXY_ERR_SOFTWARE
						end
				end;
			NoSessionFound ->
				% New session
				case find_host(State#state.rtphosts) of
					{{Node, NodeIp, []}, RtpHosts} ->
						?ERR("error no free ports at node ~p (ip ~p)!", [Node, NodeIp]),
						{?RTPPROXY_ERR_SOFTWARE, State#state{rtphosts=RtpHosts++[{Node, NodeIp, []}]}};
					{RtpHost={Node, NodeIp, [NewPort|AvailablePorts]}, RtpHosts} ->
						?INFO("Session not exists. Creating new at ~w.", [Node]),
						try rpc:call(Node, call, start, [NodeIp]) of
							{ok, CallPid} ->
								NewCallThread = #thread{pid=CallPid, callid=Cmd#cmd.callid, node=Node},
								case gen_server:call(CallPid, {Cmd#cmd.type, {NewPort, Cmd#cmd.addr, Cmd#cmd.from, Cmd#cmd.to, Cmd#cmd.params}}) of
									{ok, new, Reply} ->
										{Reply, State#state{
												calls=lists:append (State#state.calls, [NewCallThread]),
												rtphosts=RtpHosts++[{Node, NodeIp, AvailablePorts}]
											}
										};
									{ok, old, Reply} ->
										{Reply, State#state{
												calls=lists:append (State#state.calls, [NewCallThread]),
												rtphosts=RtpHosts++[RtpHost]
											}
										};
									{error, udp_error} ->
										?ERR("error in udp while CMD_U!", []),
										{?RTPPROXY_ERR_SOFTWARE, State#state{rtphosts=RtpHosts++[{Node, NodeIp, AvailablePorts}]}}
								end;
							{badrpc, nodedown} ->
								?ERR("rtp host [~w] seems stopped!", [{Node,NodeIp}]),
								% FIXME consider to remove bad host from list
								{?RTPPROXY_ERR_SOFTWARE,  State#state{rtphosts=RtpHosts++[RtpHost]}};
							Other ->
								?ERR("error creating call! [~w]", [Other]),
								{?RTPPROXY_ERR_SOFTWARE,  State#state{rtphosts=RtpHosts++[RtpHost]}}
						catch
							ExceptionClass:ExceptionPattern ->
								?ERR("Exception [~w] reached with result [~w]", [ExceptionClass, ExceptionPattern]),
								{?RTPPROXY_ERR_SOFTWARE,  State#state{rtphosts=RtpHosts++[RtpHost]}}
						end;
					error_no_nodes ->
						?ERR("error no suitable nodes (from ~p) to create new session!", [State#state.rtphosts]),
						?RTPPROXY_ERR_SOFTWARE
				end
		end
	of
		{R, NewState} ->
			gen_server:cast((Cmd#cmd.origin)#origin.pid, {reply, Cmd, R}),
			{noreply, NewState};
		R ->
			gen_server:cast((Cmd#cmd.origin)#origin.pid, {reply, Cmd, R}),
			{noreply, State}
	end;

handle_cast({message, Cmd}, State) ->
	% Unknown command
	gen_server:cast((Cmd#cmd.origin)#origin.pid, {reply, Cmd, ?RTPPROXY_ERR_SOFTWARE}),
	{noreply, State};

handle_cast({call_terminated, {Pid, {ports, Ports}, Reason}}, State) when is_list(Ports) ->
	?INFO("received call [~w] closing due to [~w] returned ~w ports", [Pid, Reason, Ports]),
	case lists:keysearch(Pid, #thread.pid, State#state.calls) of
		{value, CallThread} ->
			?INFO("call [~w] closed", [Pid]),
			case find_host_by_node(CallThread#thread.node, State#state.rtphosts) of
				false ->
					?WARN("Cannot find node and (therefore) return ports to it", []),
					{noreply, State#state{calls=lists:delete(CallThread, State#state.calls)}};
				RtpHost={Node, NodeIp, AvailablePorts} ->
					{noreply, State#state{
							calls=lists:delete(CallThread, State#state.calls),
							rtphosts=lists:delete(RtpHost, State#state.rtphosts) ++ [{Node, NodeIp, AvailablePorts ++ Ports}]
							}
					}
			end;
		false ->
			case lists:keysearch(Pid, #thread.pid, State#state.players) of
				{value, PlayerThread} ->
					?INFO("call [~w] closed", [Pid]),
					{noreply, State#state{players=lists:delete(PlayerThread, State#state.players)}};
				false ->
					?WARN("Cannot find Call or Player with such pid", []),
					{noreply, State}
			end
	end;

handle_cast({node_add, {Node, Ip, {min_port, MinPort}, {max_port, MaxPort}}}, State) when is_atom(Node), is_atom(Ip) ->
	?INFO("add node [~w]", [{Node, Ip}]),
	% TODO consider do not appending unresponsible hosts
	{noreply, State#state{rtphosts=lists:append(State#state.rtphosts, [{Node, Ip, lists:seq(MinPort, MaxPort, ?PORTS_PER_MEDIA)}])}};

handle_cast({node_del, {Node, Ip, {min_port, MinPort}, {max_port, MaxPort}}}, State) when is_atom(Node), is_atom(Ip) ->
	?INFO("del node [~w]", [{Node, Ip}]),
	{noreply, State#state{rtphosts=lists:keydelete(Node, 1 , State#state.rtphosts)}};

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
	syslog:stop();

terminate(Reason, State) ->
	?ERR("RTPPROXY terminated due to reason [~w]", [Reason]),
	syslog:stop(),
	error_logger:tty(true).

find_host_by_node(Node, RtpHosts) ->
	(utils:y(fun(F) ->
			fun	({N, []}) ->
					false;
				({N, [{N,I,P}|_Rest]}) ->
					{N,I,P};
				({N, [_Head|Tail]}) ->
					F({N,Tail})
			end
		end)
	)({Node, RtpHosts}).

find_host(Nodes) ->
	find_host(Nodes, []).

find_host([], _Acc) ->
	error_no_nodes;

find_host([{Node,Ip,Ports}|OtherNodes], Acc) ->
	Parent = self(),
	spawn (fun() ->
			Ret = net_adm:ping(Node),
			Parent ! Ret
		end),
	receive
		pong ->
			{{Node,Ip,Ports},OtherNodes ++ Acc};
		pang ->
			find_host(OtherNodes, Acc ++ [{Node,Ip,Ports}])
	after ?PING_TIMEOUT ->
			find_host(OtherNodes, Acc ++ [{Node,Ip,Ports}])
	end.
