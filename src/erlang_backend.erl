%% Copyright (c) 2011 Peter Lemenkov.
%%
%% The MIT License
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%

-module(erlang_backend).
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
	gen_server:start_link({local, backend}, ?MODULE, Args, []).

init (RtpproxyNode) ->
	% Ping every second
	{ok, TRef} = timer:send_interval(1000, ping),

	error_logger:info_msg("Erlrtpproxy backend started at ~p~n", [node()]),
	{ok, #state{timer = TRef, mode = offline, node = RtpproxyNode}}.

handle_call(_Other, _From, State) ->
	{noreply, State}.

handle_cast(stop, State) ->
	{stop, stop, State};
% Got two addresses (initial Media stream creation)
handle_cast({reply, Cmd, Answer, _}, State) ->
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
			error_logger:error_msg("No connection to erlrtpproxy.~n"),
			{noreply, State#state{mode = offline}}
	end;

handle_info(Info, State) ->
	error_logger:warning_msg("Info [~p]~n", [Info]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, #state{timer = TRef}) ->
	timer:cancel(TRef),
	error_logger:error_msg("Erlang backend stopped: ~p~n", [Reason]).
