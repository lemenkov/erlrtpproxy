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

% description of call thread
-record(callthread, {pid=null, callid=null}).

start(Args) ->
	gen_server:start({global, rtpproxy}, rtpproxy, Args, []).

start_link(Args) ->
	gen_server:start_link({global, rtpproxy}, rtpproxy, Args, []).

init([IpAtom, PortAtom]) when is_atom(IpAtom), is_atom(PortAtom) ->
	process_flag(trap_exit, true),
	{ok, Ip} = inet_parse:address(atom_to_list(IpAtom)),
	Port = list_to_integer(atom_to_list(PortAtom)),
	case gen_udp:open(Port, [{ip, Ip}, {active, true}, list]) of
		{ok, Fd} ->
			io:format("RTPProxy[~w] started at ~s:~w~n", [self(), inet_parse:ntoa(Ip), Port]),
			{ok, {Fd, []}};
		{error, Reason} ->
			io:format("RTPPROXY not started. Reason [~p]~n", Reason),
			{stop, Reason}
	end;

init(Args) ->
	io:format ("Some other [~p]", [Args]),
	{stop, "Wrong data"}.

handle_call(_Message, _From , State) ->
	{noreply, State}.

handle_cast({call_terminated, {Pid, _Reason}}, {Fd, CallsList}) ->
	case lists:keysearch(Pid, #callthread.pid, CallsList) of
		{value, CallThread} ->
			io:format("RTPPROXY call [~w] closed~n", [Pid]),
			{noreply, {Fd, lists:delete(CallThread, CallsList)}};
		false ->
			{noreply, {Fd, CallsList}}
	end;

handle_cast(_Other, State) ->
	{noreply, State}.

% Call died (due to timeout)
handle_info({'EXIT', Pid, _Reason}, {Fd, CallsList}) ->
	case lists:keysearch(Pid, #callthread.pid, CallsList) of
		{value, CallThread} ->
			io:format("RTPPROXY call [~p] closed~n", [Pid]),
			{noreply, {Fd, lists:delete(CallThread, CallsList)}};
		false ->
			{noreply, {Fd, CallsList}}
	end;

% Fd from which message arrived must be equal to Fd from our state
handle_info({udp, Fd, Ip, Port, Msg}, {Fd, CallsList}) ->
	case string:tokens(Msg, " ;") of
		[Cookie, "V"] ->
			io:format("Cookie [~s], Cmd [V]...", [Cookie]),
			MsgOut = Cookie ++ " 20040107\n",
			gen_udp:send(Fd, Ip, Port, [MsgOut]),
			io:format(" OK~n", []),
			{noreply, {Fd, CallsList}};
		[Cookie, "VF", Params] ->
			io:format("Cookie [~s], Cmd [VF] Params [~s]...", [Cookie, Params]),
			MsgOut = Cookie ++ " 1\n",
			gen_udp:send(Fd, Ip, Port, [MsgOut]),
			io:format(" OK~n", []),
			{noreply, {Fd, CallsList}};
		[Cookie, "U", CallId, OrigIp, OrigPort, FromTag, MediaId] ->
			io:format("Cmd [U] CallId [~s], OrigIp [~s], OrigPort [~s], FromTag [~s] MediaId [~s]~n", [CallId, OrigIp, OrigPort, FromTag, MediaId]),
			case lists:keysearch(CallId, #callthread.callid, CallsList) of
				{value, CallInfo} ->
					io:format("Session exists. Updating existing.~n"),
					case gen_server:call(CallInfo#callthread.pid, {message_u, {FromTag, MediaId}}) of
						{ok, Reply} ->
							MsgOut = Cookie ++ Reply,
							gen_udp:send(Fd, Ip, Port, [MsgOut]);
						{error, udp_error} ->
							MsgOut = Cookie ++ " 7\n",
							gen_udp:send(Fd, Ip, Port, [MsgOut])
					end,
					{noreply, {Fd, CallsList}};
				false ->
					io:format("Session not exists. Creating new:~n"),
					case call:start_link ([]) of
						{ok, CallPid} ->
							io:format(" OK~n"),
							NewCallThread = #callthread{pid=CallPid, callid=CallId},
							case gen_server:call(CallPid, {message_u, {FromTag, MediaId}}) of
								{ok, Reply} ->
									MsgOut = Cookie ++ Reply,
									gen_udp:send(Fd, Ip, Port, [MsgOut]),
									{noreply, {Fd, lists:append (CallsList, [NewCallThread])}};
								{error, udp_error} ->
									MsgOut = Cookie ++ " 7\n",
									gen_udp:send(Fd, Ip, Port, [MsgOut]),
									{noreply, {Fd, CallsList}}
							end;
						Other ->
							io:format ("RTPPROXY: error creating call! [~w]~n", [Other]),
							MsgOut = Cookie ++ " 7\n",
							gen_udp:send(Fd, Ip, Port, [MsgOut]),
							{noreply, {Fd, CallsList}}
					end
			end;
		[Cookie, "L", CallId, OrigIp, OrigPort, FromTag, MediaIdFrom, ToTag, MediaIdTo] ->
			io:format("Cmd [L] CallId [~s], OrigIp [~s], OrigPort [~s], FromTag [~s] MediaIdFrom [~s] ToTag [~s] MediaIdTo [~s]~n",	[CallId, OrigIp, OrigPort, FromTag, MediaIdFrom, ToTag, MediaIdTo]),
			case lists:keysearch(CallId, #callthread.callid, CallsList) of
				{value, CallInfo} ->
					case gen_server:call(CallInfo#callthread.pid, {message_l, {FromTag, MediaIdFrom, ToTag, MediaIdTo}}) of
						{error, not_found} ->
							MsgOut = Cookie ++ " 8\n",
							gen_udp:send(Fd, Ip, Port, [MsgOut]);							
						{error, udp_error} ->
							MsgOut = Cookie ++ " 7\n",
							gen_udp:send(Fd, Ip, Port, [MsgOut]);							
						{ok, Reply} ->
							MsgOut = Cookie ++ Reply,
							gen_udp:send(Fd, Ip, Port, [MsgOut])
					end;
				false ->
					io:format("Session not exists. Do nothing.~n"),
					MsgOut = Cookie ++ " 8\n",
					gen_udp:send(Fd, Ip, Port, [MsgOut])
			end,
			{noreply, {Fd, CallsList}};
		[Cookie, "D", CallId, FromTag, MediaIdFrom, ToTag, MediaIdTo] ->
			io:format("Cmd [D] CallId [~s], FromTag [~s] MediaIdFrom [~s] ToTag [~s] MediaIdTo [~s]~n", [CallId, FromTag, MediaIdFrom, ToTag, MediaIdTo]),
			case lists:keysearch(CallId, #callthread.callid, CallsList) of
				{value, CallInfo} ->
					gen_server:cast(CallInfo#callthread.pid, message_d),
					MsgOut = Cookie ++ " 0\n",
					gen_udp:send(Fd, Ip, Port, [MsgOut]);
				false ->
					io:format("Session not exists. Do nothing.~n"),
					MsgOut = Cookie ++ " 8\n",
					gen_udp:send(Fd, Ip, Port, [MsgOut])
			end,
			{noreply, {Fd, CallsList}};
		[Cookie, "R", CallId, FromTag, MediaIdFrom, ToTag, MediaIdTo] ->
			io:format("Cmd [R] CallId [~s], FromTag [~s] MediaIdFrom [~s] ToTag [~s] MediaIdTo [~s]~n", [CallId, FromTag, MediaIdFrom, ToTag, MediaIdTo]),
			case lists:keysearch(CallId, #callthread.callid, CallsList) of
				{value, CallInfo} ->
					gen_server:cast(CallInfo#callthread.pid, message_r),
					MsgOut = Cookie ++ " 0\n",
					gen_udp:send(Fd, Ip, Port, [MsgOut]);
				false ->
					io:format("Session not exists. Do nothing.~n"),
					MsgOut = Cookie ++ " 8\n",
					gen_udp:send(Fd, Ip, Port, [MsgOut])
			end,
			{noreply, {Fd, CallsList}};
		[Cookie, "S", CallId, FromTag, MediaIdFrom, ToTag, MediaIdTo] ->
			io:format("Cmd [R] CallId [~s], FromTag [~s] MediaIdFrom [~s] ToTag [~s] MediaIdTo [~s]~n", [CallId, FromTag, MediaIdFrom, ToTag, MediaIdTo]),
			case lists:keysearch(CallId, #callthread.callid, CallsList) of
				{value, CallInfo} ->
					gen_server:cast(CallInfo#callthread.pid, message_s),
					MsgOut = Cookie ++ " 0\n",
					gen_udp:send(Fd, Ip, Port, [MsgOut]);
				false ->
					io:format("Session not exists. Do nothing.~n"),
					MsgOut = Cookie ++ " 8\n",
					gen_udp:send(Fd, Ip, Port, [MsgOut])
			end,
			{noreply, {Fd, CallsList}};
		[Cookie, "I"] ->
			io:format("Cookie [~s], Cmd [I]~n", [Cookie]),
			% TODO
			{noreply, {Fd, CallsList}};
		[Cookie | _Other] ->
			io:format("Other command [~s]~n", [Msg]),
			% bad syntax
			MsgOut = Cookie ++ " 1\n",
			gen_udp:send(Fd, Ip, Port, [MsgOut]),
			{noreply, {Fd, CallsList}}
	end;

handle_info(Info, State) ->
	io:format("RTPPROXY got INFO [~p]~n", [Info]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, {Fd, _CallsList}) ->
	gen_udp:close(Fd),
	io:format("RTPPROXY terminated due to reason [~w]~n", [Reason]).

