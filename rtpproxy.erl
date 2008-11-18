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

handle_cast({message, Cmd}, State) ->
%	?INFO("Cmd[~p]", [Cmd]),
	{FinReply, FinState} = case
		% Commands with CallId
		case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.calls) of
			% Already created session
			{value, CallInfo} ->
%				?INFO("Session exists. Updating existing.", []),
				case Cmd#cmd.type of
					?CMD_U ->
						% update/create session
						case find_host_by_node(CallInfo#thread.node, State#state.rtphosts) of
							false ->
								?RTPPROXY_ERR_SOFTWARE;
							{Node, NodeIp, []} ->
								% cannot start session on that node - no available ports
								?RTPPROXY_ERR_SOFTWARE;
							RtpHost={Node, NodeIp, [NewPort|AvailablePorts]} ->
								case gen_server:call(CallInfo#thread.pid, {message_u, {NewPort, Cmd#cmd.addr, Cmd#cmd.from, Cmd#cmd.to, Cmd#cmd.params}}) of
									{ok, new, Reply} ->
										{Reply, State#state{rtphosts=lists:delete(RtpHost, State#state.rtphosts) ++ [{Node, NodeIp, AvailablePorts}]}};
									{ok, old, Reply} ->
										Reply;
									{error, udp_error} ->
										?ERR("error in udp while CMD_U!", []),
										?RTPPROXY_ERR_SOFTWARE
								end
						end;
					?CMD_L ->
						try gen_server:call(CallInfo#thread.pid, {message_l, {Cmd#cmd.addr, Cmd#cmd.from, Cmd#cmd.to, Cmd#cmd.params}}) of
							{ok, Reply} ->
								Reply;
							{error, not_found} ->
								?ERR("error not found while CMD_L!", []),
								?RTPPROXY_ERR_NOSESSION;
							{error, udp_error} ->
								?ERR("error in udp while CMD_L!", []),
								?RTPPROXY_ERR_SOFTWARE
						catch
							Error:ErrorClass ->
								?ERR("Exception {~p,~p} in udp while CMD_L!", [Error,ErrorClass]),
								?RTPPROXY_ERR_SOFTWARE
						end;
					?CMD_Q ->
						% TODO
						% sprintf(buf, "%s %d %lu %lu %lu %lu\n", cookie, spa->ttl, spa->pcount[idx], spa->pcount[NOT(idx)], spa->pcount[2], spa->pcount[3]);
						?RTPPROXY_OK;
					?CMD_C ->
						% TODO
						?RTPPROXY_OK;
					?CMD_D ->
						% Should we use call here
						gen_server:cast(CallInfo#thread.pid, message_d),
						case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.players) of
							{value, PlayerInfo} ->
								PlayerInfo#thread.pid ! message_d;
							_ ->
								ok
						end,
						?RTPPROXY_OK;
					?CMD_R ->
						gen_server:cast(CallInfo#thread.pid, {message_r, Cmd#cmd.filename}),
						?RTPPROXY_OK;
					?CMD_P ->
						case gen_server:call(CallInfo#thread.pid, {message_p, Cmd#cmd.to}) of
							{ok, Addr} ->
								case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.players) of
									{value, PlayerInfo} ->
										% Already started
										% TODO should we change played music?
										% we simply kill current thread
										PlayerInfo#thread.pid ! message_s,
										{?RTPPROXY_OK, State#state{players=lists:delete(PlayerInfo, State#state.players)}};
									false ->
										case find_host(State#state.rtphosts) of
											{RtpHost={Node, NodeIp, AvailablePorts}, RtpHosts} ->
												try rpc:call(Node, player, start, [Cmd#cmd.filename, Cmd#cmd.codecs, Addr]) of
													{ok, PlayerPid} ->
														NewPlayerThread = #thread{pid=PlayerPid, callid=Cmd#cmd.callid, node=Node},
														{?RTPPROXY_OK, State#state{
																	players=lists:append (State#state.players, [NewPlayerThread]),
																	rtphosts=RtpHosts++[RtpHost]
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
								end;
							{error, not_found} ->
								?RTPPROXY_ERR_NOSESSION
						end;
					?CMD_S ->
						gen_server:cast(CallInfo#thread.pid, message_s),
						case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.players) of
							{value, PlayerInfo} ->
								?INFO("Stop player thread.", []),
								PlayerInfo#thread.pid ! message_s,
								{?RTPPROXY_OK, State#state{players=lists:delete(PlayerInfo, State#state.players)}};
							false ->
								?ERR("CANNOT Stop player thread.", []),
								?RTPPROXY_ERR_NOSESSION
						end;
					_ ->
						?RTPPROXY_ERR_SOFTWARE
				end;
			NoSessionFound ->
				% New session - only few commands allowed here
				case Cmd#cmd.type of
					?CMD_U ->
						case find_host(State#state.rtphosts) of
							{{Node, NodeIp, []}, RtpHosts} ->
								?ERR("error no free ports at node ~p (ip ~p)!", [Node, NodeIp]),
								{?RTPPROXY_ERR_SOFTWARE, State#state{rtphosts=RtpHosts++[{Node, NodeIp, []}]}};
							{RtpHost={Node, NodeIp, [NewPort|AvailablePorts]}, RtpHosts} ->
								?INFO("Session not exists. Creating new at ~w.", [Node]),
								try rpc:call(Node, call, start, [NodeIp]) of
									{ok, CallPid} ->
										NewCallThread = #thread{pid=CallPid, callid=Cmd#cmd.callid, node=Node},
										case gen_server:call(CallPid, {message_u, {NewPort, Cmd#cmd.addr, Cmd#cmd.from, Cmd#cmd.to, Cmd#cmd.params}}) of
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
						end;
					?CMD_D ->
						case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.players) of
							{value, PlayerInfo} ->
								?INFO("Stop player thread.", []),
								PlayerInfo#thread.pid ! message_d,
								?RTPPROXY_OK;
							false ->
								?ERR("CANNOT find thread.", []),
								?RTPPROXY_ERR_NOSESSION
						end;
					?CMD_P ->
						% we must fix OpenSER-rtpproxy protocol to add ip:port of destination
						% see 'patches' directory for example
						% TODO probably we need to handle this situation separately
						% TODO contact sobomax for suggestions
						case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.players) of
							{value, PlayerInfo} ->
								% Already started
								% we simply kill current thread
								% TODO should we change played music?
								PlayerInfo#thread.pid ! message_s,
								{?RTPPROXY_OK, State#state{players=lists:delete(PlayerInfo, State#state.players)}};
							false ->
								case find_host(State#state.rtphosts) of
									{RtpHost={Node, NodeIp, AvailablePorts}, RtpHosts} ->
										{Ip, Port} = Cmd#cmd.addr,
										try rpc:call(Node, player, start, [Cmd#cmd.filename, Cmd#cmd.codecs, {null, Ip, Port}]) of
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
						end;
					?CMD_S ->
						case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.players) of
							{value, PlayerInfo} ->
								?INFO("Stop player thread.", []),
								PlayerInfo#thread.pid ! message_s,
								{?RTPPROXY_OK, State#state{players=lists:delete(PlayerInfo, State#state.players)}};
							false ->
								?ERR("Session not exists. Do nothing.", []),
								?RTPPROXY_ERR_NOSESSION
						end;
					_ ->
						?WARN("Session not exists. Do nothing.", []),
						?RTPPROXY_ERR_NOSESSION
						% TODO should we raise ERR_SOFTWARE here rather than ERR_NOSESSION
%							?RTPPROXY_ERR_SOFTWARE
				end
		end
	of
		{R, NS} ->
			{R, NS};
		R ->
			{R, State}
	end,
	gen_server:cast((Cmd#cmd.origin)#origin.pid, {reply, Cmd, FinReply}),
	{noreply, FinState};

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
	syslog:stop().

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
