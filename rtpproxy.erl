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

-define(RTPPROXY_OK, "0").

-define(RTPPROXY_ERR_SYNTAX,    "E1").
-define(RTPPROXY_ERR_SOFTWARE,  "E7").
-define(RTPPROXY_ERR_NOSESSION, "E8").

-define (MOD_LIST, [{mod_asymmetric, $A}, {mod_e, $E}, {mod_i, $I}, {mod_ipv6, $6}, {mod_symmetric, $S}, {mod_weak, $W}, {mod_z, $Z}]).

% description of call thread
-record(callthread, {pid=null, callid=null}).

start(Args) ->
	gen_server:start({global, ?MODULE}, ?MODULE, Args, []).

start_link(Args) ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, Args, []).

init([IpAtom, PortAtom]) when is_atom(IpAtom), is_atom(PortAtom) ->
	process_flag(trap_exit, true),
	{ok, Ip} = inet_parse:address(atom_to_list(IpAtom)),
	Port = list_to_integer(atom_to_list(PortAtom)),
	syslog:start(),
	case gen_udp:open(Port, [{ip, Ip}, {active, true}, list]) of
		{ok, Fd} ->
			print("RTPProxy[~w] started at ~s:~w~n", [self(), inet_parse:ntoa(Ip), Port]),
			{ok, {Fd, [], ?RtpHosts}};
		{error, Reason} ->
			print("RTPPROXY not started. Reason [~p]~n", Reason),
			{stop, Reason}
	end.

handle_call(_Message, _From , State) ->
	{noreply, State}.

handle_cast({call_terminated, {Pid, Reason}}, {Fd, CallsList, RtpHostsList}) ->
	print("RTPPROXY received call [~w] closing due to [~p]~n", [Pid, Reason]),
	case lists:keysearch(Pid, #callthread.pid, CallsList) of
		{value, CallThread} ->
			print("RTPPROXY call [~w] closed~n", [Pid]),
			{noreply, {Fd, lists:delete(CallThread, CallsList), RtpHostsList}};
		false ->
			{noreply, {Fd, CallsList, RtpHostsList}}
	end;

handle_cast({node_add, {Node, Ip}}, {Fd, CallsList, RtpHostsList}) when is_atom(Node), is_atom(Ip) ->
	print("RTPPROXY add node [~p]~n", [{Node, Ip}]),
	{noreply, {Fd, CallsList, lists:append(RtpHostsList, [{Node, Ip}])}};

handle_cast({node_del, {Node, Ip}}, {Fd, CallsList, RtpHostsList}) when is_atom(Node), is_atom(Ip) ->
	print("RTPPROXY del node [~p]~n", [{Node, Ip}]),
	{noreply, {Fd, CallsList, lists:delete({Node, Ip}, RtpHostsList)}};

handle_cast(_Other, State) ->
	{noreply, State}.

% Call died (due to timeout)
handle_info({'EXIT', Pid, _Reason}, {Fd, CallsList, RtpHostsList}) ->
	case lists:keysearch(Pid, #callthread.pid, CallsList) of
		{value, CallThread} ->
			print("RTPPROXY call [~p] closed~n", [Pid]),
			{noreply, {Fd, lists:delete(CallThread, CallsList), RtpHostsList}};
		false ->
			{noreply, {Fd, CallsList, RtpHostsList}}
	end;

% Fd from which message arrived must be equal to Fd from our state
% Brief introbuction of protocol is here: http://rtpproxy.org/wiki/RTPproxyProtocol
handle_info({udp, Fd, Ip, Port, Msg}, {Fd, CallsList, RtpHostsList}) ->
	[Cookie|Rest] = string:tokens(Msg, " ;"),
	[Answer|Addon] = case Rest of
		% Request basic supported rtpproxy protocol version
		["V"] ->
			print ("Cookie [~s], Cmd [V]~n", [Cookie]),
			["20040107"];
		% Request additional rtpproxy protocol extensions
		["VF", Params] ->
			print ("Cookie [~s], Cmd [VF] Params [~s]~n", [Cookie, Params]),
				% TODO we should check version capabilities here
			["1"];
		% update/create session
		[[$U|Args], CallId, OrigIp, OrigPort, FromTag, MediaId] ->
			{Modifiers, _} = lists:unzip(lists:filter(fun({_, Sym}) -> lists:member(Sym, Args) end, ?MOD_LIST)),
			print("Cmd [U] CallId [~s], OrigAddr [~s:~s], FromTag [~s;~s]~n", [CallId, OrigIp, OrigPort, FromTag, MediaId]),
			case lists:keysearch(CallId, #callthread.callid, CallsList) of
				{value, CallInfo} ->
					print ("Session exists. Updating existing.~n", []),
					case gen_server:call(CallInfo#callthread.pid, {message_u, {OrigIp, OrigPort, FromTag, MediaId, Modifiers}}) of
						{ok, Reply} ->
							[Reply];
						{error, udp_error} ->
							[?RTPPROXY_ERR_SOFTWARE]
					end;
				false ->
					case find_node(RtpHostsList) of
						{{RtpHost, RtpIp}, NewRtpHostsList} ->
							print("Session not exists. Creating new at ~p.~n", [RtpHost]),
							try rpc:call(RtpHost, call, start, [RtpIp]) of
								{ok, CallPid} ->
									NewCallThread = #callthread{pid=CallPid, callid=CallId},
									case gen_server:call(CallPid, {message_u, {OrigIp, OrigPort, FromTag, MediaId}}) of
										{ok, Reply} ->
											[Reply, NewRtpHostsList, lists:append (CallsList, [NewCallThread])];
										{error, udp_error} ->
											[?RTPPROXY_ERR_SOFTWARE, NewRtpHostsList];
										{Other} ->
											print("Other msg [~p]~n", [Other]),
											[?RTPPROXY_ERR_SOFTWARE]
									end;
								{badrpc, nodedown} ->
									print ("RTPPROXY: rtp host [~p] seems stopped!~n", [{RtpHost,RtpIp}]),
									% FIXME remove bad host from list
									[?RTPPROXY_ERR_SOFTWARE, NewRtpHostsList];
								Other ->
									print ("RTPPROXY: error creating call! [~w]~n", [Other]),
									[?RTPPROXY_ERR_SOFTWARE, NewRtpHostsList]
							catch
								Exception ->
									print("Exception reached [~p]~n", [Exception]),
									[?RTPPROXY_ERR_SOFTWARE, NewRtpHostsList]
							end;
						error_no_node ->
							print ("RTPPROXY: error no suitable nodes!~n", []),
							[?RTPPROXY_ERR_SOFTWARE]
					end
			end;
		% lookup existing session
		[[$L|Args], CallId, OrigIp, OrigPort, FromTag, FromMediaId, ToTag, ToMediaId] ->
			{Modifiers, _} = lists:unzip(lists:filter(fun({_, Sym}) -> lists:member(Sym, Args) end, ?MOD_LIST)),
			print ("Cmd [L] CallId [~s], OrigAddr [~s:~s], FromTag [~s;~s] ToTag [~s;~s]~n", [CallId, OrigIp, OrigPort, FromTag, FromMediaId, ToTag, ToMediaId]),
			case lists:keysearch(CallId, #callthread.callid, CallsList) of
				{value, CallInfo} ->
					case gen_server:call(CallInfo#callthread.pid, {message_l, {FromTag, FromMediaId, ToTag, ToMediaId, Modifiers}}) of
						{error, not_found} ->
							[?RTPPROXY_ERR_NOSESSION];
						{error, udp_error} ->
							[?RTPPROXY_ERR_SOFTWARE];
						{ok, Reply} ->
							[Reply]
					end;
				false ->
					print("RTPPROXY Session not exists. Do nothing.~n"),
					[?RTPPROXY_ERR_NOSESSION]
			end;
		% delete session
		["D", CallId, FromTag, FromMediaId, ToTag, ToMediaId] ->
			print ("Cmd [D] CallId [~s], FromTag [~s;~s] ToTag [~s;~s]~n", [CallId, FromTag, FromMediaId, ToTag, ToMediaId]),
			case lists:keysearch(CallId, #callthread.callid, CallsList) of
				{value, CallInfo} ->
					gen_server:cast(CallInfo#callthread.pid, message_d),
					[?RTPPROXY_OK];
				false ->
					print("RTPPROXY Session not exists. Do nothing.~n"),
					[?RTPPROXY_ERR_NOSESSION]
			end;
		% record
		["R", CallId, FromTag, FromMediaId, ToTag, ToMediaId] ->
			print ("Cmd [R] CallId [~s], FromTag [~s;~s] ToTag [~s;~s]~n", [CallId, FromTag, FromMediaId, ToTag, ToMediaId]),
			case lists:keysearch(CallId, #callthread.callid, CallsList) of
				{value, CallInfo} ->
					gen_server:cast(CallInfo#callthread.pid, message_r),
					[?RTPPROXY_OK];
				false ->
					print("RTPPROXY Session not exists. Do nothing.~n"),
					[?RTPPROXY_ERR_NOSESSION]
			end;
		% playback pre-recorded audio
		["P", CallId, PlayName, Codecs, FromTag, FromMediaId, ToTag, ToMediaId] ->
			print ("Cmd [P] CallId [~s], FromTag [~s;~s] ToTag [~s;~s]~n", [CallId, FromTag, FromMediaId, ToTag, ToMediaId]),
			case lists:keysearch(CallId, #callthread.callid, CallsList) of
				{value, CallInfo} ->
					gen_server:cast(CallInfo#callthread.pid, message_p),
					[?RTPPROXY_OK];
				false ->
					print("RTPPROXY Session not exists. Do nothing.~n"),
					[?RTPPROXY_ERR_NOSESSION]
			end;
		% stop playback or record
		["S", CallId, FromTag, FromMediaId, ToTag, ToMediaId] ->
			print ("Cmd [R] CallId [~s], FromTag [~s;~s] ToTag [~s;~s]~n", [CallId, FromTag, FromMediaId, ToTag, ToMediaId]),
			case lists:keysearch(CallId, #callthread.callid, CallsList) of
				{value, CallInfo} ->
					gen_server:cast(CallInfo#callthread.pid, message_s),
					[?RTPPROXY_OK];
				false ->
					print("RTPRPXY Session not exists. Do nothing.~n"),
					[?RTPPROXY_ERR_NOSESSION]
			end;
		% copy session (same as record?)
		["C", CallId, RecordName, FromTag, FromMediaId, ToTag, ToMediaId] ->
			print("RTPPROXY Cookie [~s], Cmd [C]~n", [Cookie]),
			% TODO
			[?RTPPROXY_OK];
		% query
		["Q", CallId, FromTag, FromMediaId, ToTag, ToMediaId] ->
			print("RTPPROXY Cookie [~s], Cmd [Q]~n", [Cookie]),
			% TODO
			% sprintf(buf, "%s %d %lu %lu %lu %lu\n", cookie, spa->ttl, spa->pcount[idx], spa->pcount[NOT(idx)], spa->pcount[2], spa->pcount[3]);
			[?RTPPROXY_OK];
		% stop all active sessions
		["X"] ->
			print("RTPPROXY Cookie [~s], Cmd [X]~n", [Cookie]),
			lists:foreach(fun(X) -> gen_server:cast(X#callthread.pid, message_d) end, CallsList),
			[?RTPPROXY_OK];
		["I"] ->
			print("RTPPROXY Cookie [~s], Cmd [I]~n", [Cookie]),
			% TODO show information about calls
			Stats = lists:map(fun(X) -> gen_server:call(X#callthread.pid, message_i) end, CallsList),
			% "sessions created: %llu\nactive sessions: %d\n active streams: %d\n"
			% foreach session "%s/%s: caller = %s:%d/%s, callee = %s:%d/%s, stats = %lu/%lu/%lu/%lu, ttl = %d/%d\n"
			[?RTPPROXY_OK];
		[_Other] ->
			print ("RTPPROXY Other command (bad syntax?) [~s]~n", [Msg]),
			[?RTPPROXY_ERR_SYNTAX]
	end,
	gen_udp:send(Fd, Ip, Port, Cookie ++ " " ++  Answer ++ "\n"),
	case Addon of
		[NewRtpHostsList1, NewCallsList1] ->
			{noreply, {Fd, NewCallsList1, NewRtpHostsList1}};
		[NewRtpHostsList1] ->
			{noreply, {Fd, CallsList, NewRtpHostsList1}};
		[] ->
			{noreply, {Fd, CallsList, RtpHostsList}}
	end;

handle_info(Info, State) ->
	print("RTPPROXY got INFO [~p]~n", [Info]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, {Fd, CallsList, RtpHostsList}) ->
	gen_udp:close(Fd),
	syslog:stop(),
	print("RTPPROXY terminated due to reason [~w]~n", [Reason]).

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
	syslog:send(rtpproxy, syslog:info(), io_lib:format(Format, Params)).
%	io:format(Format, Params).
