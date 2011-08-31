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

-module(ser).
-author('lemenkov@gmail.com').

-behaviour(gen_server).
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include("common.hrl").

-record(state, {timer, mode, node}).

start_link(Args) ->
	gen_server:start_link({local, ser}, ?MODULE, Args, []).

init (_Unused) ->
	process_flag(trap_exit, true),

	% Load parameters
	{ok, {Proto, IpStr, Port}} = application:get_env(?MODULE, listen),
	{ok, Ip} = inet_parse:address(IpStr),

	RtpproxyNode = case application:get_env(?MODULE, rtpproxy_node) of
		undefined -> undefined;
		{ok, RtpproxyNode0} -> RtpproxyNode0
	end,

	% Ping every second
	{ok, TRef} = timer:send_interval(1000, ping),

	listener_sup:start_link(Proto, self(), Ip, Port),

	error_logger:info_msg("SER nathelper interface started at ~p~n", [node()]),
	{ok, #state{timer = TRef, mode = offline, node = RtpproxyNode}}.

handle_call(_Other, _From, State) ->
	{noreply, State}.

% Got two addresses (initial Media stream creation)
handle_cast({reply, Cmd, Answer, _}, State) ->
	gen_server:cast(listener, {Cmd, Answer}),
	{noreply, State};
% TODO deprecate this case
handle_cast({reply, Cmd, Answer}, State) ->
	gen_server:cast(listener, {Cmd, Answer}),
	{noreply, State};

handle_cast(#cmd{type = ?CMD_V} = Cmd, State) ->
	% Request basic supported rtpproxy protocol version
	% see available versions here:
	% http://sippy.git.sourceforge.net/git/gitweb.cgi?p=sippy/rtpproxy;a=blob;f=rtpp_command.c#l58
	% We provide only basic functionality, currently.
	error_logger:info_msg("SER cmd V~n"),
	gen_server:cast(listener, {Cmd, {version, "20040107"}}),
	{noreply, State};
handle_cast(#cmd{type = ?CMD_VF, params=Version} = Cmd, State) ->
	% Request additional rtpproxy protocol extensions
	error_logger:info_msg("SER cmd VF: ~s~n", [Version]),
	gen_server:cast(listener, {Cmd, {supported, Version}}),
	{noreply, State};
handle_cast(#cmd{origin = Origin} = Cmd, #state{mode = online} = State) ->
	error_logger:info_msg("SER cmd: ~p~n", [Cmd]),
	gen_server:cast({global, rtpproxy}, Cmd#cmd{origin = Origin#origin{pid = self()}}),
	{noreply, State};
handle_cast(Cmd, #state{mode = offline} = State) ->
	error_logger:info_msg("SER cmd (OFFLINE): ~p~n", [Cmd]),
	gen_server:cast(listener, {Cmd, {error, software}}),
	{noreply, State};

handle_cast(_Request, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, #state{timer = TRef}) ->
	timer:cancel(TRef),
	error_logger:info_msg("SER nathelper interface terminated due to reason ~p~n", [Reason]).

handle_info(ping, #state{node = undefined} = State) ->
	{noreply, State#state{mode = offline}};
handle_info(ping, #state{mode = Online, node = RtpproxyNode} = State) ->
	case net_adm:ping(RtpproxyNode) of
		pong when Online == offline ->
			error_logger:warning_msg("Connection to erlrtpproxy restored.~n"),
			{noreply, State#state{mode = online}};
		pong ->
			{noreply, State#state{mode = online}};
		pang ->
			error_logger:error_msg("Lost connection to erlrtpproxy.~n"),
			{noreply, State#state{mode = offline}}
	end;

handle_info(Info, State) ->
	error_logger:warning_msg("Info [~w]~n", [Info]),
	{noreply, State}.
