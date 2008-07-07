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
-include ("config.hrl").
-include("common.hrl").

% description of call thread
-record(callthread, {pid=null, callid=null}).
-record(state, {calls=null, rtphosts=null}).

start(Args) ->
	gen_server:start({global, ?MODULE}, ?MODULE, Args, []).

start_link(Args) ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, Args, []).

init(_Unused) ->
	process_flag(trap_exit, true),
	syslog:start(),
	print("RTPProxy[~w] started with rtphosts [~p]~n", [self(), ?RtpHosts]),
	{ok, #state{calls=[], rtphosts=?RtpHosts}}.

handle_call({message, Cmd}, From, State) ->
	print("Cmd[~p]~n", [Cmd]),
	case Cmd#cmd.type of
		% Request basic supported rtpproxy protocol version
		?CMD_V ->
			{reply, "20040107", State};
		% Request additional rtpproxy protocol extensions
		?CMD_VF ->
			% TODO we should check version capabilities here
			{reply, "1", State};
		% stop all active sessions
		?CMD_X ->
			lists:foreach(fun(X) -> gen_server:cast(X#callthread.pid, message_d) end, State#state.calls),
			{reply, ?RTPPROXY_OK, State};
		?CMD_I ->
			% TODO show information about calls
			Stats = lists:map(fun(X) -> gen_server:call(X#callthread.pid, message_i) end, State#state.calls),
			% "sessions created: %llu\nactive sessions: %d\n active streams: %d\n"
			% foreach session "%s/%s: caller = %s:%d/%s, callee = %s:%d/%s, stats = %lu/%lu/%lu/%lu, ttl = %d/%d\n"
			{reply, ?RTPPROXY_OK, State};
		_Other ->
			% Commands with CallId
			case lists:keysearch(Cmd#cmd.callid, #callthread.callid, State#state.calls) of
				{value, CallInfo} ->
					print ("Session exists. Updating existing.~n"),
					case Cmd#cmd.type of
						?CMD_U ->
							% update/create session
							case gen_server:call(CallInfo#callthread.pid, {message_u, {Cmd#cmd.addr, Cmd#cmd.from, Cmd#cmd.params}}) of
								{ok, Reply} ->
									{reply, Reply, State};
								{error, udp_error} ->
									{reply, ?RTPPROXY_ERR_SOFTWARE, State}
							end;
						?CMD_L ->
							case gen_server:call(CallInfo#callthread.pid, {message_l, {Cmd#cmd.from, Cmd#cmd.to, Cmd#cmd.params}}) of
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
							gen_server:cast(CallInfo#callthread.pid, message_d),
							{reply, ?RTPPROXY_OK, State};
						?CMD_R ->
							gen_server:cast(CallInfo#callthread.pid, {message_r, Cmd#cmd.filename}),
							{reply, ?RTPPROXY_OK, State};
						?CMD_P ->
							gen_server:cast(CallInfo#callthread.pid, {message_p, Cmd#cmd.filename}),
							{reply, ?RTPPROXY_OK, State};
						?CMD_S ->
							gen_server:cast(CallInfo#callthread.pid, message_s),
							{reply, ?RTPPROXY_OK, State};
						_ ->
							{reply, ?RTPPROXY_ERR_SOFTWARE, State}
					end;
				false ->
					case Cmd#cmd.type of
						?CMD_U ->
							case find_node(State#state.rtphosts) of
								{{RtpHost, RtpIp}, RtpHosts} ->
									print("Session not exists. Creating new at ~p.~n", [RtpHost]),
									try rpc:call(RtpHost, call, start, [RtpIp]) of
										{ok, CallPid} ->
											NewCallThread = #callthread{pid=CallPid, callid=Cmd#cmd.callid},
											case gen_server:call(CallPid, {message_u, {Cmd#cmd.addr, Cmd#cmd.from, Cmd#cmd.params}}) of
												{ok, Reply} ->
													{reply, Reply, State#state{calls=lists:append (State#state.calls, [NewCallThread]), rtphosts=RtpHosts}};
												{error, udp_error} ->
													{reply, ?RTPPROXY_ERR_SOFTWARE,  State#state{rtphosts=RtpHosts}};
												{Other} ->
													print("Other msg [~p]~n", [Other]),
													{reply, ?RTPPROXY_ERR_SOFTWARE, State}
											end;
										{badrpc, nodedown} ->
											print ("RTPPROXY: rtp host [~p] seems stopped!~n", [{RtpHost,RtpIp}]),
											% FIXME consider to remove bad host from list
											{reply, ?RTPPROXY_ERR_SOFTWARE,  State#state{rtphosts=RtpHosts}};
										Other ->
											print ("RTPPROXY: error creating call! [~w]~n", [Other]),
											{reply, ?RTPPROXY_ERR_SOFTWARE,  State#state{rtphosts=RtpHosts}}
									catch
										ExceptionClass:ExceptionPattern ->
											print("Exception [~p] reached with result [~p]~n", [ExceptionClass, ExceptionPattern]),
											{reply, ?RTPPROXY_ERR_SOFTWARE,  State#state{rtphosts=RtpHosts}}
									end;
								error_no_nodes ->
									print ("RTPPROXY: error no suitable nodes to create new session!~n"),
									{reply, ?RTPPROXY_ERR_SOFTWARE, State}
							end;
						_ ->
							print("Session not exists. Do nothing.~n"),
							{reply, ?RTPPROXY_ERR_NOSESSION, State}
					end
			end
	end;

handle_call(_Message, _From , State) ->
	{reply, ?RTPPROXY_ERR_SOFTWARE, State}.

handle_cast({call_terminated, {Pid, Reason}}, State) ->
	print("RTPPROXY received call [~w] closing due to [~p]~n", [Pid, Reason]),
	case lists:keysearch(Pid, #callthread.pid, State#state.calls) of
		{value, CallThread} ->
			print("RTPPROXY call [~w] closed~n", [Pid]),
			{noreply, State#state{calls=lists:delete(CallThread, State#state.calls)}};
		false ->
			{noreply, State}
	end;

handle_cast({node_add, {Node, Ip}}, State) when is_atom(Node), is_atom(Ip) ->
	print("RTPPROXY add node [~p]~n", [{Node, Ip}]),
	% TODO consider do not appending unresponcible hosts
	{noreply, State#state{rtphosts=lists:append(State#state.rtphosts, [{Node, Ip}])}};

handle_cast({node_del, {Node, Ip}}, State) when is_atom(Node), is_atom(Ip) ->
	print("RTPPROXY del node [~p]~n", [{Node, Ip}]),
	{noreply, State#state{rtphosts=lists:delete({Node, Ip}, State#state.rtphosts)}};

handle_cast(_Other, State) ->
	{noreply, State}.

% Call died (due to timeout)
handle_info({'EXIT', Pid, _Reason}, State) ->
	case lists:keysearch(Pid, #callthread.pid, State#state.calls) of
		{value, CallThread} ->
			print("RTPPROXY call [~p] closed~n", [Pid]),
			{noreply, State#state{calls=lists:delete(CallThread, State#state.calls)}};
		false ->
			{noreply, State}
	end;

handle_info(Info, State) ->
	print("RTPPROXY got INFO [~p]~n", [Info]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, State) ->
	syslog:stop(),
	io:format("RTPPROXY terminated due to reason [~p]~n", [Reason]).

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

print (Format) ->
	print (Format, []).

print (Format, Params) ->
%	io:format(Format, Params),
	syslog:send(rtpproxy, syslog:info(), io_lib:format(Format, Params)).
