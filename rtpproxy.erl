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
-record(thread, {pid=null, callid=null}).
-record(state, {calls=[], rtphosts=null, players=[]}).

start(Args) ->
	gen_server:start({global, ?MODULE}, ?MODULE, Args, []).

start_link(Args) ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, Args, []).

init(_Unused) ->
	process_flag(trap_exit, true),
	syslog:start(),
	?PRINT("started with rtphosts [~w]", [?RtpHosts]),
	{ok, #state{rtphosts=?RtpHosts}}.

handle_call({message, Cmd}, From, State) ->
%	?PRINT("Cmd[~p]", [Cmd]),
	case Cmd#cmd.type of
		% Request basic supported rtpproxy protocol version
		?CMD_V ->
%			{ "20040107", "Basic RTP proxy functionality" },
%			{ "20050322", "Support for multiple RTP streams and MOH" },
%			{ "20060704", "Support for extra parameter in the V command" },
%			{ "20071116", "Support for RTP re-packetization" },
%			{ "20071218", "Support for forking (copying) RTP stream" },
%			{ "20080403", "Support for RTP statistics querying" },
			{reply, "20040107", State};
		% Request additional rtpproxy protocol extensions
		?CMD_VF ->
			% TODO we should check version capabilities here
			{reply, "1", State};
		% stop all active sessions
		?CMD_X ->
			lists:foreach(fun(X) -> gen_server:cast(X#thread.pid, message_d) end, State#state.calls),
			lists:foreach(fun(X) -> gen_server:cast(X#thread.pid, message_d) end, State#state.players),
			{reply, ?RTPPROXY_OK, State};
		?CMD_I ->
			% TODO show information about calls
			Stats = lists:map(fun(X) -> gen_server:call(X#thread.pid, message_i) end, State#state.calls),
			% "sessions created: %llu\nactive sessions: %d\n active streams: %d\n"
			% foreach session "%s/%s: caller = %s:%d/%s, callee = %s:%d/%s, stats = %lu/%lu/%lu/%lu, ttl = %d/%d\n"
			{reply, ?RTPPROXY_OK, State};
		_Other ->
			% Commands with CallId
			case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.calls) of
				% Already created session
				{value, CallInfo} ->
%					?PRINT("Session exists. Updating existing.", []),
					case Cmd#cmd.type of
						?CMD_U ->
							% update/create session
							case gen_server:call(CallInfo#thread.pid, {message_u, {Cmd#cmd.addr, Cmd#cmd.from, Cmd#cmd.to, Cmd#cmd.params}}) of
								{ok, Reply} ->
									{reply, Reply, State};
								{error, udp_error} ->
									{reply, ?RTPPROXY_ERR_SOFTWARE, State}
							end;
						?CMD_L ->
							case gen_server:call(CallInfo#thread.pid, {message_l, {Cmd#cmd.addr, Cmd#cmd.from, Cmd#cmd.to, Cmd#cmd.params}}) of
								{ok, Reply} ->
									{reply, Reply, State};
								{error, not_found} ->
									{reply, ?RTPPROXY_ERR_NOSESSION, State};
								{error, udp_error} ->
									{reply, ?RTPPROXY_ERR_SOFTWARE, State}
							end;
						?CMD_Q ->
							% TODO
							% sprintf(buf, "%s %d %lu %lu %lu %lu\n", cookie, spa->ttl, spa->pcount[idx], spa->pcount[NOT(idx)], spa->pcount[2], spa->pcount[3]);
							{reply, ?RTPPROXY_OK, State};
						?CMD_C ->
							% TODO
							{reply, ?RTPPROXY_OK, State};
						?CMD_D ->
							gen_server:cast(CallInfo#thread.pid, message_d),
							case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.players) of
								{value, PlayerInfo} ->
									PlayerInfo#thread.pid ! message_d;
								_ ->
									ok
							end,
							{reply, ?RTPPROXY_OK, State};
						?CMD_R ->
							gen_server:cast(CallInfo#thread.pid, {message_r, Cmd#cmd.filename}),
							{reply, ?RTPPROXY_OK, State};
						?CMD_P ->
							case gen_server:call(CallInfo#thread.pid, {message_p, Cmd#cmd.to}) of
								{ok, Addr} ->
									case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.players) of
										{value, PlayerInfo} ->
											% Already started
											% TODO should we change played music?
											% we simply kill current thread
											PlayerInfo#thread.pid ! message_s,
											{reply, ?RTPPROXY_OK, State#state{players=lists:delete(PlayerInfo, State#state.players)}};
		%									{reply, ?RTPPROXY_OK, State};
										false ->
											case find_node(State#state.rtphosts) of
												{{RtpHost, RtpIp}, RtpHosts} ->
													try rpc:call(RtpHost, player, start, [Cmd#cmd.filename, Cmd#cmd.codecs, Addr]) of
														{ok, PlayerPid} ->
															NewPlayerThread = #thread{pid=PlayerPid, callid=Cmd#cmd.callid},
															{reply, ?RTPPROXY_OK, State#state{players=lists:append (State#state.players, [NewPlayerThread]), rtphosts=RtpHosts}};
														{badrpc, nodedown} ->
															?PRINT("rtp host [~w] seems stopped!", [{RtpHost,RtpIp}]),
															% FIXME consider to remove bad host from list
															{reply, ?RTPPROXY_ERR_SOFTWARE,  State#state{rtphosts=RtpHosts}};
														Other ->
															?PRINT("error creating call! [~w]", [Other]),
															{reply, ?RTPPROXY_ERR_SOFTWARE,  State#state{rtphosts=RtpHosts}}
													catch
														ExceptionClass:ExceptionPattern ->
															?PRINT("Exception [~w] reached with result [~w]", [ExceptionClass, ExceptionPattern]),
															{reply, ?RTPPROXY_ERR_SOFTWARE,  State#state{rtphosts=RtpHosts}}
													end;
												error_no_nodes ->
													?PRINT("error no suitable nodes to create new player!", []),
													{reply, ?RTPPROXY_ERR_SOFTWARE, State}
											end
									end;
								{error, not_found} ->
									{reply, ?RTPPROXY_ERR_NOSESSION, State}
							end;
						?CMD_S ->
							gen_server:cast(CallInfo#thread.pid, message_s),
							case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.players) of
								{value, PlayerInfo} ->
									?PRINT("Stop player thread.", []),
									PlayerInfo#thread.pid ! message_s,
									{reply, ?RTPPROXY_OK, State#state{players=lists:delete(PlayerInfo, State#state.players)}};
								false ->
									?PRINT("CANNOT Stop player thread.", []),
									{reply, ?RTPPROXY_ERR_NOSESSION, State}
							end;
						_ ->
							{reply, ?RTPPROXY_ERR_SOFTWARE, State}
					end;
				false ->
					% New session
					case Cmd#cmd.type of
						?CMD_U ->
							case find_node(State#state.rtphosts) of
								{{RtpHost, RtpIp}, RtpHosts} ->
									?PRINT("Session not exists. Creating new at ~w.", [RtpHost]),
									try rpc:call(RtpHost, call, start, [RtpIp]) of
										{ok, CallPid} ->
											NewCallThread = #thread{pid=CallPid, callid=Cmd#cmd.callid},
											case gen_server:call(CallPid, {message_u, {Cmd#cmd.addr, Cmd#cmd.from, Cmd#cmd.to, Cmd#cmd.params}}) of
												{ok, Reply} ->
													{reply, Reply, State#state{calls=lists:append (State#state.calls, [NewCallThread]), rtphosts=RtpHosts}};
												{error, udp_error} ->
													{reply, ?RTPPROXY_ERR_SOFTWARE,  State#state{rtphosts=RtpHosts}};
												{Other} ->
													?PRINT("Other msg [~w]", [Other]),
													{reply, ?RTPPROXY_ERR_SOFTWARE, State}
											end;
										{badrpc, nodedown} ->
											?PRINT("rtp host [~w] seems stopped!", [{RtpHost,RtpIp}]),
											% FIXME consider to remove bad host from list
											{reply, ?RTPPROXY_ERR_SOFTWARE,  State#state{rtphosts=RtpHosts}};
										Other ->
											?PRINT("error creating call! [~w]", [Other]),
											{reply, ?RTPPROXY_ERR_SOFTWARE,  State#state{rtphosts=RtpHosts}}
									catch
										ExceptionClass:ExceptionPattern ->
											?PRINT("Exception [~w] reached with result [~w]", [ExceptionClass, ExceptionPattern]),
											{reply, ?RTPPROXY_ERR_SOFTWARE,  State#state{rtphosts=RtpHosts}}
									end;
								error_no_nodes ->
									?PRINT("error no suitable nodes to create new session!", []),
									{reply, ?RTPPROXY_ERR_SOFTWARE, State}
							end;
						?CMD_D ->
							case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.players) of
								{value, PlayerInfo} ->
									?PRINT("Stop player thread.", []),
									PlayerInfo#thread.pid ! message_d,
									{reply, ?RTPPROXY_OK, State};
								false ->
									?PRINT("CANNOT Stop player thread.", []),
									{reply, ?RTPPROXY_ERR_NOSESSION, State}
							end;
						?CMD_P ->
							% TODO we must fix OpenSER-rtpproxy protocol to add ip:port of destination
							case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.players) of
								{value, PlayerInfo} ->
									% Already started
									% TODO should we change played music?
									% we simply kill current thread
									PlayerInfo#thread.pid ! message_s,
									{reply, ?RTPPROXY_OK, State#state{players=lists:delete(PlayerInfo, State#state.players)}};
%									{reply, ?RTPPROXY_OK, State};
								false ->
									case find_node(State#state.rtphosts) of
										{{RtpHost, RtpIp}, RtpHosts} ->
											{Ip1, Port1} = Cmd#cmd.addr,
											try rpc:call(RtpHost, player, start, [Cmd#cmd.filename, Cmd#cmd.codecs, {null, Ip1, Port1}]) of
												{ok, PlayerPid} ->
													NewPlayerThread = #thread{pid=PlayerPid, callid=Cmd#cmd.callid},
													{reply, ?RTPPROXY_OK, State#state{players=lists:append (State#state.players, [NewPlayerThread]), rtphosts=RtpHosts}};
												{badrpc, nodedown} ->
													?PRINT("rtp host [~w] seems stopped!", [{RtpHost,RtpIp}]),
													% FIXME consider to remove bad host from list
													{reply, ?RTPPROXY_ERR_SOFTWARE,  State#state{rtphosts=RtpHosts}};
												Other ->
													?PRINT("error creating call! [~w]", [Other]),
													{reply, ?RTPPROXY_ERR_SOFTWARE,  State#state{rtphosts=RtpHosts}}
											catch
												ExceptionClass:ExceptionPattern ->
													?PRINT("Exception [~w] reached with result [~w]", [ExceptionClass, ExceptionPattern]),
													{reply, ?RTPPROXY_ERR_SOFTWARE,  State#state{rtphosts=RtpHosts}}
											end;
										error_no_nodes ->
											?PRINT("error no suitable nodes to create new player!", []),
											{reply, ?RTPPROXY_ERR_SOFTWARE, State}
									end
							end;
						?CMD_S ->
							case lists:keysearch(Cmd#cmd.callid, #thread.callid, State#state.players) of
								{value, PlayerInfo} ->
									?PRINT("Stop player thread.", []),
									PlayerInfo#thread.pid ! message_s,
									{reply, ?RTPPROXY_OK, State#state{players=lists:delete(PlayerInfo, State#state.players)}};
								false ->
									?PRINT("CANNOT Stop player thread.", []),
									{reply, ?RTPPROXY_ERR_NOSESSION, State}
							end;
						_ ->
							?PRINT("Session not exists. Do nothing.", []),
							{reply, ?RTPPROXY_ERR_NOSESSION, State}
					end
			end
	end;

handle_call(_Message, _From , State) ->
	{reply, ?RTPPROXY_ERR_SOFTWARE, State}.

handle_cast({call_terminated, {Pid, Reason}}, State) ->
	?PRINT("received call [~w] closing due to [~w]", [Pid, Reason]),
	case lists:keysearch(Pid, #thread.pid, State#state.calls) of
		{value, CallThread} ->
			?PRINT("call [~w] closed", [Pid]),
			{noreply, State#state{calls=lists:delete(CallThread, State#state.calls)}};
		false ->
			case lists:keysearch(Pid, #thread.pid, State#state.players) of
				{value, PlayerThread} ->
					?PRINT("call [~w] closed", [Pid]),
					{noreply, State#state{players=lists:delete(PlayerThread, State#state.players)}};
				false ->
					{noreply, State}
			end
	end;

handle_cast({node_add, {Node, Ip}}, State) when is_atom(Node), is_atom(Ip) ->
	?PRINT("add node [~w]", [{Node, Ip}]),
	% TODO consider do not appending unresponsible hosts
	{noreply, State#state{rtphosts=lists:append(State#state.rtphosts, [{Node, Ip}])}};

handle_cast({node_del, {Node, Ip}}, State) when is_atom(Node), is_atom(Ip) ->
	?PRINT("del node [~w]", [{Node, Ip}]),
	{noreply, State#state{rtphosts=lists:delete({Node, Ip}, State#state.rtphosts)}};

handle_cast(_Other, State) ->
	{noreply, State}.

% Call died (due to timeout)
handle_info({'EXIT', Pid, _Reason}, State) ->
	case lists:keysearch(Pid, #thread.pid, State#state.calls) of
		{value, CallThread} ->
			?PRINT("call [~w] closed", [Pid]),
			{noreply, State#state{calls=lists:delete(CallThread, State#state.calls)}};
		false ->
			{noreply, State}
	end;

handle_info(Info, State) ->
	case Info of
		{udp, Fd, Ip, Port, Data} ->
			{ok, {LocalIp, LocalPort}} = inet:sockname(Fd),
			?PRINT("got udp INFO (shouldn't happend) from [~w ~w] to [~w, ~w, ~w] <<some data>> of size ~w", [Ip, Port, Fd, LocalIp, LocalPort, size(Data)]);
		_ ->
			?PRINT("got INFO [~w]", [Info])
	end,
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, State) ->
	syslog:stop(),
	io:format("RTPPROXY terminated due to reason [~w]", [Reason]).

find_node (Nodes) ->
	find_node(Nodes, []).

find_node([], _Acc) ->
	error_no_nodes;

find_node([{Node,Ip}|OtherNodes], Acc) ->
	Parent = self(),
	spawn (fun() ->
			Ret = net_adm:ping(Node),
			Parent ! Ret
		end),
	receive
		pong ->
			{{Node,Ip},OtherNodes ++ Acc ++ [{Node,Ip}]};
		pang ->
			find_node(OtherNodes, Acc ++ [{Node,Ip}])
	after ?PING_TIMEOUT ->
			find_node(OtherNodes, Acc ++ [{Node,Ip}])
	end.
