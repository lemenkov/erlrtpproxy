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

-module(backend_ser).
-author('lemenkov@gmail.com').

-export([command/5]).
-export([reply/2]).

-include("common.hrl").

command(Msg, Fd, Ip, Port, Begin) ->
	try ser_proto:decode(Msg) of
		#cmd{cookie = Cookie, type = ?CMD_V} ->
			% Request basic supported rtpproxy protocol version
			% see available versions here:
			% http://sippy.git.sourceforge.net/git/gitweb.cgi?p=sippy/rtpproxy;a=blob;f=rtpp_command.c#l58
			% We provide only basic functionality, currently.
			error_logger:info_msg("SER backend: cmd V~n"),
%			Data = ser_proto:encode(#response{cookie = Cookie, origin = Origin, type = reply, data = {version, <<"20040107">>}}),
			{<<Cookie/binary, " 20040107\n">>, Ip, Port};
		#cmd{cookie = Cookie, type = ?CMD_VF, params=Version} ->
			% Request additional rtpproxy protocol extensions
			error_logger:info_msg("SER backend: cmd VF: ~s~n", [Version]),
%			Data = ser_proto:encode(#response{cookie = Cookie, origin = Origin, type = reply, data = supported}),
			{<<Cookie/binary, " 1\n">>, Ip, Port};
		#cmd{origin = Origin, type = ?CMD_L} = Cmd ->
			error_logger:info_msg("SER backend: cmd: ~p~n", [Cmd]),
			spawn(rtpproxy_ctl, command, [Cmd#cmd{origin = Origin#origin{fd = Fd, ip=Ip, port=Port}, timestamp = Begin}]),
			ok;
		#cmd{origin = Origin, type = ?CMD_U} = Cmd ->
			error_logger:info_msg("SER backend: cmd: ~p~n", [Cmd]),
			NotifyParams = proplists:get_value(notify, Cmd#cmd.params),
			case NotifyParams of
				undefined ->
					spawn(rtpproxy_ctl, command, [Cmd#cmd{origin = Origin#origin{fd = Fd, ip=Ip, port=Port}, timestamp = Begin}]);
				_ ->
					case proplists:get_value(addr, NotifyParams) of
						{_,_} ->
							spawn(rtpproxy_ctl, command, [Cmd#cmd{origin = Origin#origin{fd = Fd, ip=Ip, port=Port}, timestamp = Begin}]);
						P when is_integer(P) ->
							NotifyTag = proplists:get_value(tag, NotifyParams),
							% Assume that the IP is the same as the origin of command
							NewNotifyParams = [{notify, [{addr, {Ip, P}}, {tag, NotifyTag}]}],
							NewParams = proplists:delete(notify, Cmd#cmd.params) ++ NewNotifyParams,
							spawn(rtpproxy_ctl, command, [Cmd#cmd{origin = Origin#origin{fd = Fd, ip=Ip, port=Port}, params = NewParams, timestamp = Begin}])
					end
			end,
			ok;
		#cmd{cookie = Cookie, origin = Origin} = Cmd ->
			error_logger:info_msg("SER backend: cmd: ~p~n", [Cmd]),
			Ret =  rtpproxy_ctl:command(Cmd#cmd{origin = Origin#origin{fd = Fd, ip=Ip, port=Port}, timestamp = Begin}),
			case Ret of
				{ok, {stats, Number}} ->
					error_logger:info_msg("SER backend: reply stats (short)~n"),
					Data = ser_proto:encode(#response{cookie = Cookie, origin = Origin, type = reply, data = Ret}),
					{Data, Ip, Port};
				{ok, {stats, NumberTotal, NumberActive}} ->
					error_logger:info_msg("SER backend: reply stats (full)~n"),
					Data = ser_proto:encode(#response{cookie = Cookie, origin = Origin, type = reply, data = {ok, {stats, NumberTotal, NumberActive}}}),
					{Data, Ip, Port};
				ok ->
					error_logger:info_msg("SER backend: reply ok (~p)~n", [Cmd]),
					Data = ser_proto:encode(#response{cookie = Cookie, origin = Origin, type = reply, data = ok}),
					{Data, Ip, Port};
				{error, notfound} ->
					error_logger:info_msg("SER backend: reply {error, notfound) (~p)~n", [Cmd]),
					Data = ser_proto:encode(#response{cookie = Cookie, origin = Origin, type = error, data = notfound}),
					{Data, Ip, Port};
				_ ->
					error_logger:info_msg("SER backend: cmd RET: ~p~n", [Ret]),
					ok
			end
	catch
		throw:{error_syntax, ErrorMsg} when is_list(ErrorMsg) ->
			error_logger:warning_msg("SER backend: error bad syntax. [~s -> ~s]~n", [Msg, ErrorMsg]),
			Data = ser_proto:encode({error, syntax, Msg}),
			{Data, Ip, Port};
		throw:{error_syntax, {ErrorMsg, ErrorData}} when is_list(ErrorMsg) ->
			error_logger:warning_msg("SER backend: error bad syntax. [~s -> ~s==~p]~n", [Msg, ErrorMsg, ErrorData]),
			Data = ser_proto:encode({error, syntax, Msg}),
			{Data, Ip, Port};
		E:C ->
			error_logger:warning_msg("SER backend: error exception. [~s -> ~p:~p]~n", [Msg, E, C]),
			Data = ser_proto:encode({error, syntax, Msg}),
			{Data, Ip, Port}
	end.

reply(Cmd = #cmd{cookie = Cookie, callid = CallId, timestamp = TS, origin = #origin{type = ser, fd = {Type, Fd}, ip = Ip, port = Port}}, {{Ip0, Port0}, _}) ->
	I = list_to_binary(inet_parse:ntoa(Ip0)),
	P = list_to_binary(integer_to_list(Port0)),
	case Type of
		udp -> prim_inet:sendto(Fd, Ip, Port, <<Cookie/binary, " ", P/binary, " ", I/binary, "\n">>);
		tcp -> prim_inet:send(Fd, <<Cookie/binary, " ", P/binary, " ", I/binary, "\n">>)
	end,
	End = {MegaSecs1, Secs1, MicroSecs1} = os:timestamp(),
	error_logger:info_msg("SER backend: reply ~s ~s sent to ~s:~b at ~f (elapsed time: ~b microsec)~n", [<<Cookie/binary, " ", P/binary, " ", I/binary, "\n">>, CallId, inet_parse:ntoa(Ip), Port, MegaSecs1*1000000+Secs1+MicroSecs1/1000000, timer:now_diff(End, TS)]),
	ok.
