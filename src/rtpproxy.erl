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
-export([start/0]).
-export([start/1]).
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include("../include/common.hrl").

% description of call thread
-record(thread, {pid=null, id=null}).
-record(state, {calls=[]}).

start() ->
	gen_server:start({global, ?MODULE}, ?MODULE, [], []).

start(Args) ->
	gen_server:start({global, ?MODULE}, ?MODULE, Args, []).

start_link(Args) ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, Args, []).

init(_Unused) ->
	?INFO("rtpproxy started at ~p", [node()]),
	{ok, #state{}}.

handle_call(status, _From, #state{calls = Calls} = State) ->
	Header = io_lib:format("Current state - ~p media stream(s):~n", [length(Calls)]),
	?INFO(Header, []),
	MediaInfos = lists:map(fun(X) ->
			% TODO fix this strange situation
			{CallId, MediaId} = X#thread.id,
			{ok, Reply} = try gen_server:call(X#thread.pid, ?CMD_Q) catch _:_ -> {ok, [["died (shouldn't happend)"]]} end,
			MediaInfo = io_lib:format("* CallID: ~s, MediaId: ~p, ~s~n", [CallId, MediaId, Reply]),
			?INFO(MediaInfo, []),
			MediaInfo
		end,
	Calls),
	Footer = "Current state: END.",
	?INFO(Footer, []),
	{reply, [Header] ++ MediaInfos ++ [Footer], State};

handle_call(_Message, _From , State) ->
	{reply, error, State}.

handle_cast(#cmd{type = ?CMD_V, origin = #origin{pid = Pid}} = Cmd, State) ->
	% Request basic supported rtpproxy protocol version
	% see available versions here:
	% http://sippy.git.sourceforge.net/git/gitweb.cgi?p=sippy/rtpproxy;a=blob;f=rtpp_command.c#l58
	% We, curently, provide only basic functionality
	gen_server:cast(Pid, {reply, Cmd, {version, "20040107"}}),
	{noreply, State};

handle_cast(#cmd{type = ?CMD_VF, origin = #origin{pid = Pid}, params=Version} = Cmd, State) ->
	% Request additional rtpproxy protocol extensions
	% TODO we should check version capabilities here
	gen_server:cast(Pid, {reply, Cmd, {supported, Version}}),
	{noreply, State};

handle_cast(#cmd{type = ?CMD_X, origin = #origin{pid = Pid}} = Cmd, State) ->
	% stop all active sessions
	lists:foreach(fun(X) -> gen_server:cast(X#thread.pid, stop) end, State#state.calls),
	gen_server:cast(Pid, {reply, Cmd, ok}),
	{noreply, State};

handle_cast(#cmd{type = ?CMD_I, origin = #origin{pid = Pid}} = Cmd, State) ->
	% TODO show information about calls
	Stats = lists:map(fun(X) -> gen_server:call(X#thread.pid, ?CMD_Q) end, State#state.calls),
	% "sessions created: %llu\nactive sessions: %d\n active streams: %d\n"
	% foreach session "%s/%s: caller = %s:%d/%s, callee = %s:%d/%s, stats = %lu/%lu/%lu/%lu, ttl = %d/%d\n"
	gen_server:cast(Pid, {reply, Cmd, ok}),
	{noreply, State};

% First try to find existing session
handle_cast(#cmd{origin = #origin{pid = Pid}, callid = CallId, mediaid = MediaId} = Cmd, #state{calls = Calls} = State) ->
	case get_media(CallId, MediaId, Calls) of
		{value, MediaThread} ->
			% Operate on existing media thread
			gen_server:cast(MediaThread#thread.pid, Cmd);
		{list, MediaThreads} ->
			lists:foreach(fun(X) -> gen_server:cast(X#thread.pid, Cmd) end, MediaThreads),
			% FIXME is it a correct behaviour?
			gen_server:cast(Pid, {reply, Cmd, ok});
		false when Cmd#cmd.type == ?CMD_U ->
			% Create new media thread
			?INFO("Media stream does not exist. Creating new.", []),
			pool:pspawn(media, start, [Cmd]);
		false ->
			?WARN("Media stream does not exist. Do nothing.", []),
			gen_server:cast(Pid, {reply, Cmd, {error, notfound}})
	end,
	{noreply, State};

handle_cast({created, CallPid, {CallId, MediaId}}, #state{calls = Calls} = State) ->
	{noreply, State#state{calls=lists:append(Calls, [#thread{pid=CallPid, id={CallId, MediaId}}])}};

% Call died (due to timeout)
handle_cast({'EXIT', Pid, Reason}, #state{calls = Calls} = State) ->
	case lists:keysearch(Pid, #thread.pid, Calls) of
		{value, CallThread} ->
			?INFO("received 'EXIT' from ~p due to [~p]", [Pid, Reason]),
			{noreply, State#state{calls=lists:delete(CallThread, Calls)}};
		false ->
			{noreply, State}
	end;

handle_cast(Other, State) ->
	?WARN("Other cast [~p], State [~p]", [Other, State]),
	{noreply, State}.

handle_info(Other, State) ->
	?WARN("Other Info [~p], State [~p]", [Other, State]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate({ErrorClass, {Module,Function, [Pid, Message]}}, _State) ->
	?ERR("RTPPROXY terminated due to Error [~p] in ~p:~p(...) with Msg[~p] from Pid ~p", [ErrorClass, Module, Function, Message, Pid]);

terminate(Reason, _State) ->
	?ERR("RTPPROXY terminated due to reason [~w]", [Reason]).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions %%
%%%%%%%%%%%%%%%%%%%%%%%%

get_media(CallId, 0, Calls) ->
	List = lists:filter(fun(X) -> {C, _} = X#thread.id, C == CallId  end, Calls),
	{list, List};
get_media(CallId, MediaId, Calls) ->
	lists:keysearch({CallId, MediaId}, #thread.id, Calls).
