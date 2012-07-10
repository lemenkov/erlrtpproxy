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

-module(backend).
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

-include("common.hrl").

-record(state, {timer, mode, node}).

start(Args) ->
	gen_server:start({local, backend}, ?MODULE, Args, []).
start_link(Args) ->
	gen_server:start_link({local, backend}, ?MODULE, Args, []).

init (RtpproxyNode) ->
	% Ping every second
	{ok, TRef} = timer:send_interval(1000, ping),

	error_logger:info_msg("Erlrtpproxy backend started at ~p~n", [node()]),
	{ok, #state{timer = TRef, mode = offline, node = RtpproxyNode}}.

handle_call(Other, _From, State) ->
	error_logger:warning_msg("Erlrtpproxy backend: strange call: ~p~n", [Other]),
	{noreply, State}.

handle_cast(stop, State) ->
	{stop, stop, State};

handle_cast({reply, Cmd = #cmd{origin = #origin{type = ser, ip = Ip, port = Port}}, ok}, State) ->
	error_logger:info_msg("SER reply ok~n"),
	Data = ser_proto:encode(#response{cookie = Cmd#cmd.cookie, origin = Cmd#cmd.origin, type = reply, data = ok}),
	gen_server:cast(listener, {msg, Data, Ip, Port}),
	{noreply, State};

handle_cast({reply, Cmd = #cmd{origin = #origin{type = ser, ip = Ip, port = Port}}, {stats, Number}}, State) ->
	error_logger:info_msg("SER reply stats (short)~n"),
	Data = ser_proto:encode(#response{cookie = Cmd#cmd.cookie, origin = Cmd#cmd.origin, type = reply, data = {ok, {stats, Number}}}),
	gen_server:cast(listener, {msg, Data, Ip, Port}),
	{noreply, State};

handle_cast({reply, Cmd = #cmd{origin = #origin{type = ser, ip = Ip, port = Port}}, {stats, NumberTotal, NumberActive}}, State) ->
	error_logger:info_msg("SER reply stats (full)~n"),
	Data = ser_proto:encode(#response{cookie = Cmd#cmd.cookie, origin = Cmd#cmd.origin, type = reply, data = {ok, {stats, NumberTotal, NumberActive}}}),
	gen_server:cast(listener, {msg, Data, Ip, Port}),
	{noreply, State};

handle_cast({reply, Cmd = #cmd{origin = #origin{type = ser, ip = Ip, port = Port}}, {Addr1, Addr2}}, State) ->
	error_logger:info_msg("SER reply ~p~n", [{Addr1, Addr2}]),
	Data = ser_proto:encode(#response{cookie = Cmd#cmd.cookie, origin = Cmd#cmd.origin, type = reply, data = {Addr1, Addr2}}),
	gen_server:cast(listener, {msg, Data, Ip, Port}),
	{noreply, State};

handle_cast({msg, Msg, Ip, Port}, #state{mode = offline} = State) ->
	error_logger:info_msg("SER cmd (OFFLINE): ~p~n", [Msg]),
	Data = ser_proto:encode({error, software, Msg}),
	gen_server:cast(listener, {msg, Data, Ip, Port}),
	{noreply, State};

handle_cast({msg, Msg, Ip, Port}, State) ->
	try ser_proto:decode(Msg) of
		#cmd{cookie = Cookie, origin = Origin, type = ?CMD_V} ->
			% Request basic supported rtpproxy protocol version
			% see available versions here:
			% http://sippy.git.sourceforge.net/git/gitweb.cgi?p=sippy/rtpproxy;a=blob;f=rtpp_command.c#l58
			% We provide only basic functionality, currently.
			error_logger:info_msg("SER cmd V~n"),
			Data = ser_proto:encode(#response{cookie = Cookie, origin = Origin, type = reply, data = {version, <<"20040107">>}}),
			gen_server:cast(listener, {msg, Data, Ip, Port});
		#cmd{cookie = Cookie, origin = Origin, type = ?CMD_VF, params=Version} ->
			% Request additional rtpproxy protocol extensions
			error_logger:info_msg("SER cmd VF: ~s~n", [Version]),
			Data = ser_proto:encode(#response{cookie = Cookie, origin = Origin, type = reply, data = supported}),
			gen_server:cast(listener, {msg, Data, Ip, Port});
		#cmd{origin = Origin, type = ?CMD_L} = Cmd ->
			error_logger:info_msg("SER cmd: ~p~n", [Cmd]),
			rtpproxy_ctl:command(Cmd#cmd{origin = Origin#origin{ip=Ip, port=Port}, type = ?CMD_U});
		#cmd{origin = Origin, type = ?CMD_U} = Cmd ->
			error_logger:info_msg("SER cmd: ~p~n", [Cmd]),
			NotifyParams = proplists:get_value(notify, Cmd#cmd.params),
			case NotifyParams of
				undefined ->
					rtpproxy_ctl:command(Cmd#cmd{origin = Origin#origin{ip=Ip, port=Port}});
				_ ->
					case proplists:get_value(addr, NotifyParams) of
						{_,_} ->
							rtpproxy_ctl:command(Cmd#cmd{origin = Origin#origin{ip=Ip, port=Port}});
						P when is_integer(P) ->
							NotifyTag = proplists:get_value(tag, NotifyParams),
							% Assume that the IP is the same as the origin of command
							NewNotifyParams = [{notify, [{addr, {Ip, P}}, {tag, NotifyTag}]}],
							NewParams = proplists:delete(notify, Cmd#cmd.params) ++ NewNotifyParams,
							rtpproxy_ctl:command(Cmd#cmd{origin = Origin#origin{ip=Ip, port=Port}, params = NewParams})
					end
			end;
		#cmd{origin = Origin} = Cmd ->
			error_logger:info_msg("SER cmd: ~p~n", [Cmd]),
			rtpproxy_ctl:command(Cmd#cmd{origin = Origin#origin{ip=Ip, port=Port}}),
			error_logger:info_msg("SER reply ok~n"),
			Data = ser_proto:encode(#response{cookie = Cmd#cmd.cookie, origin = Cmd#cmd.origin, type = reply, data = ok}),
			gen_server:cast(listener, {msg, Data, Ip, Port})
	catch
		throw:{error_syntax, ErrorMsg} when is_list(ErrorMsg) ->
			error_logger:error_msg("Bad syntax. [~s -> ~s]~n", [Msg, ErrorMsg]),
			Data = ser_proto:encode({error, syntax, Msg}),
			gen_server:cast(listener, {msg, Data, Ip, Port});
		throw:{error_syntax, {ErrorMsg, ErrorData}} when is_list(ErrorMsg) ->
			error_logger:error_msg("Bad syntax. [~s -> ~s==~p]~n", [Msg, ErrorMsg, ErrorData]),
			Data = ser_proto:encode({error, syntax, Msg}),
			gen_server:cast(listener, {msg, Data, Ip, Port});
		E:C ->
			error_logger:error_msg("Exception. [~s -> ~p:~p]~n", [Msg, E, C]),
			Data = ser_proto:encode({error, syntax, Msg}),
			gen_server:cast(listener, {msg, Data, Ip, Port})
	end,
	{noreply, State};

handle_cast(Other, State) ->
	error_logger:warning_msg("Erlrtpproxy backend: strange cast: ~p~n", [Other]),
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
	error_logger:warning_msg("Erlrtpproxy backend: strange info: ~p~n", [Info]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, #state{timer = TRef}) ->
	timer:cancel(TRef),
	error_logger:error_msg("Erlang backend stopped: ~p~n", [Reason]).
