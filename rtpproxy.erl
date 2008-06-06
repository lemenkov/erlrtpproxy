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
		["V"] ->
			print ("Cookie [~s], Cmd [V]~n", [Cookie]),
			["20040107"];
		["VF", Params] ->
			print ("Cookie [~s], Cmd [VF] Params [~s]~n", [Cookie, Params]),
			["1"];
		[[$U|Args], CallId, OrigIp, OrigPort, FromTag, MediaId] ->
			print("Cmd [U] CallId [~s], OrigAddr [~s:~s], FromTag [~s;~s]~n", [CallId, OrigIp, OrigPort, FromTag, MediaId]),
			case lists:keysearch(CallId, #callthread.callid, CallsList) of
				{value, CallInfo} ->
					print ("Session exists. Updating existing.~n", []),
					case gen_server:call(CallInfo#callthread.pid, {message_u, {OrigIp, OrigPort, FromTag, MediaId}}) of
						{ok, Reply} ->
							[Reply];
						{error, udp_error} ->
							["7"]
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
											[Reply, lists:append (CallsList, [NewCallThread]), NewRtpHostsList];
										{error, udp_error} ->
											["7", NewRtpHostsList];
										{Other} ->
											print("Other msg [~p]~n", [Other]),
											["7"]
									end;
								{badrpc, nodedown} ->
									print ("RTPPROXY: rtp host [~p] seems stopped!~n", [{RtpHost,RtpIp}]),
									% FIXME remove bad host from list
									["7", NewRtpHostsList];
								Other ->
									print ("RTPPROXY: error creating call! [~w]~n", [Other]),
									["7", NewRtpHostsList]
							catch
								Exception ->
									print("Exception reached [~p]~n", [Exception]),
									["7", NewRtpHostsList]
							end;
						error_no_node ->
							print ("RTPPROXY: error no suitable nodes!~n", []),
							["7"]
					end
			end;
		[[$L|Args], CallId, OrigIp, OrigPort, FromTag, MediaIdFrom, ToTag, MediaIdTo] ->
			print ("Cmd [L] CallId [~s], OrigAddr [~s:~s], FromTag [~s;~s] ToTag [~s;~s]~n", [CallId, OrigIp, OrigPort, FromTag, MediaIdFrom, ToTag, MediaIdTo]),
			case lists:keysearch(CallId, #callthread.callid, CallsList) of
				{value, CallInfo} ->
					case gen_server:call(CallInfo#callthread.pid, {message_l, {FromTag, MediaIdFrom, ToTag, MediaIdTo}}) of
						{error, not_found} ->
							["8"];
						{error, udp_error} ->
							["7"];
						{ok, Reply} ->
							[Reply]
					end;
				false ->
					print("RTPPROXY Session not exists. Do nothing.~n"),
					["8"]
			end;
		["D", CallId, FromTag, MediaIdFrom, ToTag, MediaIdTo] ->
			print ("Cmd [D] CallId [~s], FromTag [~s;~s] ToTag [~s;~s]~n", [CallId, FromTag, MediaIdFrom, ToTag, MediaIdTo]),
			case lists:keysearch(CallId, #callthread.callid, CallsList) of
				{value, CallInfo} ->
					gen_server:cast(CallInfo#callthread.pid, message_d),
					["0"];
				false ->
					print("RTPPROXY Session not exists. Do nothing.~n"),
					["8"]
			end;
		["R", CallId, FromTag, MediaIdFrom, ToTag, MediaIdTo] ->
			print ("Cmd [R] CallId [~s], FromTag [~s;~s] ToTag [~s;~s]~n", [CallId, FromTag, MediaIdFrom, ToTag, MediaIdTo]),
			case lists:keysearch(CallId, #callthread.callid, CallsList) of
				{value, CallInfo} ->
					gen_server:cast(CallInfo#callthread.pid, message_r),
					["0"];
				false ->
					print("RTPPROXY Session not exists. Do nothing.~n"),
					["8"]
			end;
		[[$P|Args], CallId, PlayName, Codecs, FromTag, MediaIdFrom, ToTag, MediaIdTo] ->
			print ("Cmd [P] CallId [~s], FromTag [~s;~s] ToTag [~s;~s]~n", [CallId, FromTag, MediaIdFrom, ToTag, MediaIdTo]),
			% TODO
			["0"];
		["S", CallId, FromTag, MediaIdFrom, ToTag, MediaIdTo] ->
			print ("Cmd [R] CallId [~s], FromTag [~s;~s] ToTag [~s;~s]~n", [CallId, FromTag, MediaIdFrom, ToTag, MediaIdTo]),
			case lists:keysearch(CallId, #callthread.callid, CallsList) of
				{value, CallInfo} ->
					gen_server:cast(CallInfo#callthread.pid, message_s),
					["0"];
				false ->
					print("RTPRPXY Session not exists. Do nothing.~n"),
					["8"]
			end;
		["I"] ->
			print("RTPPROXY Cookie [~s], Cmd [I]~n", [Cookie]),
			% TODO show information about calls
			["0"];
		[_Other] ->% bad syntax
			print ("RTPPROXY Other command [~s]~n", [Msg]),
			["1"]
	end,
	gen_udp:send(Fd, Ip, Port, Cookie ++ " " ++  Answer ++ "\n"),
	case Addon of
		[NewCallsList1, NewRtpHostsList1] ->
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
