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
handle_info({udp, Fd, Ip, Port, Msg}, {Fd, CallsList, RtpHostsList}) ->
	Fun = fun (Format, Params, F) ->
		print(Format, Params),
		gen_udp:send(Fd, Ip, Port, [F()]),
		{noreply, {Fd, CallsList, RtpHostsList}}
	end,

	case string:tokens(Msg, " ;") of
		[Cookie, "V"] ->
			Fun (	"Cookie [~s], Cmd [V]~n",
				[Cookie],
				fun () -> Cookie ++ " 20040107\n" end
			);
		[Cookie, "VF", Params] ->
			Fun (	"Cookie [~s], Cmd [VF] Params [~s]~n",
				[Cookie, Params],
				fun () -> Cookie ++ " 1\n" end
			);
		[Cookie, [$U|Args], CallId, OrigIp, OrigPort, FromTag, MediaId] ->
			print("Cmd [U] CallId [~s], OrigAddr [~s:~s], FromTag [~s;~s]~n", [CallId, OrigIp, OrigPort, FromTag, MediaId]),
			case lists:keysearch(CallId, #callthread.callid, CallsList) of
				{value, CallInfo} ->
					Fun (	"Session exists. Updating existing.~n",
						[],
						fun () -> case gen_server:call(CallInfo#callthread.pid, {message_u, {OrigIp, OrigPort, FromTag, MediaId}}) of
								{ok, Reply} ->
									Cookie ++ Reply;
								{error, udp_error} ->
									Cookie ++ " 7\n"
							end
						end
					);
				false ->
					case find_node(RtpHostsList) of
						{{RtpHost, RtpIp}, NewRtpHostsList} ->
							print("Session not exists. Creating new at ~p.~n", [RtpHost]),
							try rpc:call(RtpHost, call, start, [RtpIp]) of
								{ok, CallPid} ->
									NewCallThread = #callthread{pid=CallPid, callid=CallId},
									case gen_server:call(CallPid, {message_u, {OrigIp, OrigPort, FromTag, MediaId}}) of
										{ok, Reply} ->
											MsgOut = Cookie ++ Reply,
											gen_udp:send(Fd, Ip, Port, [MsgOut]),
											{noreply, {Fd, lists:append (CallsList, [NewCallThread]), NewRtpHostsList}};
										{error, udp_error} ->
											MsgOut = Cookie ++ " 7\n",
											gen_udp:send(Fd, Ip, Port, [MsgOut]),
											{noreply, {Fd, CallsList, NewRtpHostsList}};
										{Other} ->
											print("Other msg [~p]~n", [Other])
									end;
								{badrpc, nodedown} ->
									print ("RTPPROXY: rtp host [~p] seems stopped!~n", [{RtpHost,RtpIp}]),
									% FIXME remove bad host from list
									MsgOut = Cookie ++ " 7\n",
									gen_udp:send(Fd, Ip, Port, [MsgOut]),
									{noreply, {Fd, CallsList, NewRtpHostsList}};
								Other ->
									print ("RTPPROXY: error creating call! [~w]~n", [Other]),
									MsgOut = Cookie ++ " 7\n",
									gen_udp:send(Fd, Ip, Port, [MsgOut]),
									{noreply, {Fd, CallsList, NewRtpHostsList}}
							catch
								Exception ->
									print("Exception reached [~p]~n", [Exception]),
									MsgOut = Cookie ++ " 7\n",
									gen_udp:send(Fd, Ip, Port, [MsgOut]),
									{noreply, {Fd, CallsList, NewRtpHostsList}}
							end;
						error_no_node ->
							Fun (	"RTPPROXY: error no suitable nodes!~n",
								[],
								fun () -> Cookie ++ " 7\n" end
							)
					end
			end;
		[Cookie, [$L|Args], CallId, OrigIp, OrigPort, FromTag, MediaIdFrom, ToTag, MediaIdTo] ->
			Fun (	"Cmd [L] CallId [~s], OrigAddr [~s:~s], FromTag [~s;~s] ToTag [~s;~s]~n",
				[CallId, OrigIp, OrigPort, FromTag, MediaIdFrom, ToTag, MediaIdTo],
				fun () -> case lists:keysearch(CallId, #callthread.callid, CallsList) of
						{value, CallInfo} ->
							case gen_server:call(CallInfo#callthread.pid, {message_l, {FromTag, MediaIdFrom, ToTag, MediaIdTo}}) of
								{error, not_found} ->
									Cookie ++ " 8\n";
								{error, udp_error} ->
									Cookie ++ " 7\n";
								{ok, Reply} ->
									Cookie ++ Reply
							end;
						false ->
							print("RTPPROXY Session not exists. Do nothing.~n"),
							Cookie ++ " 8\n"
					end
				end
			);
		[Cookie, "D", CallId, FromTag, MediaIdFrom, ToTag, MediaIdTo] ->
			Fun  (	"Cmd [D] CallId [~s], FromTag [~s;~s] ToTag [~s;~s]~n",
				[CallId, FromTag, MediaIdFrom, ToTag, MediaIdTo],
				fun () -> case lists:keysearch(CallId, #callthread.callid, CallsList) of
						{value, CallInfo} ->
							gen_server:cast(CallInfo#callthread.pid, message_d),
							Cookie ++ " 0\n";
						false ->
							print("RTPPROXY Session not exists. Do nothing.~n"),
							Cookie ++ " 8\n"
					end
				end
			);
		[Cookie, "R", CallId, FromTag, MediaIdFrom, ToTag, MediaIdTo] ->
			Fun (	"Cmd [R] CallId [~s], FromTag [~s;~s] ToTag [~s;~s]~n",
				[CallId, FromTag, MediaIdFrom, ToTag, MediaIdTo],
				fun () -> case lists:keysearch(CallId, #callthread.callid, CallsList) of
						{value, CallInfo} ->
							gen_server:cast(CallInfo#callthread.pid, message_r),
							Cookie ++ " 0\n";
						false ->
							print("RTPPROXY Session not exists. Do nothing.~n"),
							Cookie ++ " 8\n"
					end
				end
			);
		[Cookie, [$P|Args], CallId, PlayName, Codecs, FromTag, MediaIdFrom, ToTag, MediaIdTo] ->
			print("Cmd [P] CallId [~s], FromTag [~s;~s] ToTag [~s;~s]~n", [CallId, FromTag, MediaIdFrom, ToTag, MediaIdTo]),
			% TODO
			{noreply, {Fd, CallsList, RtpHostsList}};
		[Cookie, "S", CallId, FromTag, MediaIdFrom, ToTag, MediaIdTo] ->
			Fun (	"Cmd [R] CallId [~s], FromTag [~s;~s] ToTag [~s;~s]~n",
				[CallId, FromTag, MediaIdFrom, ToTag, MediaIdTo],
				fun () -> case lists:keysearch(CallId, #callthread.callid, CallsList) of
						{value, CallInfo} ->
							gen_server:cast(CallInfo#callthread.pid, message_s),
							Cookie ++ " 0\n";
						false ->
							print("RTPRPXY Session not exists. Do nothing.~n"),
							Cookie ++ " 8\n"
					end
				end
			);
		[Cookie, "I"] ->
			print("RTPPROXY Cookie [~s], Cmd [I]~n", [Cookie]),
			% TODO
			{noreply, {Fd, CallsList, RtpHostsList}};
		[Cookie | _Other] ->% bad syntax
			Fun  (	"RTPPROXY Other command [~s]~n",
				[Msg],
				fun () -> Cookie ++ " 1\n" end
			)
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
