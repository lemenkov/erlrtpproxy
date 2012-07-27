%%%----------------------------------------------------------------------
%%% Copyright (c) 2012 Peter Lemenkov <lemenkov@gmail.com>
%%%
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification,
%%% are permitted provided that the following conditions are met:
%%%
%%% * Redistributions of source code must retain the above copyright notice, this
%%% list of conditions and the following disclaimer.
%%% * Redistributions in binary form must reproduce the above copyright notice,
%%% this list of conditions and the following disclaimer in the documentation
%%% and/or other materials provided with the distribution.
%%% * Neither the name of the authors nor the names of its contributors
%%% may be used to endorse or promote products derived from this software
%%% without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ''AS IS'' AND ANY
%%% EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
%%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%
%%%----------------------------------------------------------------------

-module(rtpproxy_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/common.hrl").

-define(RTPPROXY_IP, {127,0,0,1}).
-define(RTPPROXY_PORT, 33333).

rtpproxy_test_() ->

	%%
	%% This is the socket which will be used for sending commands and receiving notifications messages
	%%

	{ok, Fd} = gen_udp:open(0, [{active, false}, binary]),
	{ok, {_, Port}} = inet:sockname(Fd),

	{setup,
		fun() ->
				%%
				%% Set node name
				%%

				net_kernel:start(['erlrtpproxy_test@localhost', longnames]),

				%%
				%% Set necessary options
				%% (normally we'll set them in the /etc/erlrtpproxy.config
				%%

				%% Set up backend's type (SER for now)
				application:set_env(rtpproxy, backend, ser),

				%% Options for SER backend
				application:set_env(rtpproxy, listen, {udp, "127.0.0.1", ?RTPPROXY_PORT}),

				%% Options for notification backend
				%application:set_env(rtpproxy, radacct_servers, [[?RTPPROXY_IP,1813,"testradacctpass"]]),
				application:set_env(rtpproxy, notify_servers, udp),
				application:set_env(rtpproxy, ignore_start, true),
				application:set_env(rtpproxy, ignore_stop, true),

				%% Options for rtpproxy itself
				application:set_env(rtpproxy, external, ?RTPPROXY_IP),
				application:set_env(rtpproxy, ttl, 105000),

				%%
				%% Start rtpproxy
				%%

				rtpproxy_ctl:start()
		end,
		fun (_) ->
				gen_udp:close(Fd),
				application:stop(rtpproxy),
				application:stop(gproc),
				net_kernel:stop()
		end,
		[
			{"Try to handshake (get magic number back -20040107)",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"592_36821 V\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"592_36821 20040107\n">>, Answer) end
			},
			{"Request basic RTP proxy functionality (ver. 20040107)",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"6721_50320 VF 20040107\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"6721_50320 1\n">>, Answer) end
			},
			{"Request support for multiple RTP streams and MOH (ver. 20050322)",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"6721_09219 VF 20050322\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"6721_09219 1\n">>, Answer) end
			},
			{"Request support for extra parameter in the V command (ver. 20060704)",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"6721_86921 VF 20060704\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"6721_86921 1\n">>, Answer) end
			},
			{"Request support for RTP re-packetization (ver. 20071116)",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"6721_19382 VF 20071116\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"6721_19382 1\n">>, Answer) end
			},
			{"Request support for forking (copying) RTP stream (ver. 20071218)",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"195345 VF 20071218\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"195345 1\n">>, Answer) end
			},
			{"Request support for RTP statistics querying (ver. 20080403)",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"782361 VF 20080403\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"782361 1\n">>, Answer) end
			},
			{"Request support for setting codecs in the update/lookup command (ver. 20081102)",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"456987 VF 20081102\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"456987 1\n">>, Answer) end
			},
			{"Request support for session timeout notifications (ver. 20081224)",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"145698 VF 20081224\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"145698 1\n">>, Answer) end
			},
			{"Request support for automatic bridging (ver. 20090810)",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"352743 VF 20090810\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"352743 1\n">>, Answer) end
			},
			{"Request for unsupported extensions",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"809210 VF 20111109\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						% A generic rtpproxy returns 0 here which looks wrong. Should be E1 really.
						?assertEqual(<<"809210 E1\n">>, Answer) end
			},
			{"Try to create new session",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"24393_4 Uc0,8,18,101 0003e30c-callid01@192.168.0.100 192.0.43.10 27686 0003e30cc50cd69210b8c36b-0ecf0120;1\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						?assertMatch(
							#response{
								cookie = <<"24393_4">>,
								origin = null,
								type = reply,
								data = {{{_,_,_,_},_},{{_,_,_,_},_}}
							},
							ser_proto:decode(Answer)) end
			},
			{"Try to lookup existing session",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"24393_4 Lc0,8,18,101 0003e30c-callid01@192.168.0.100 192.0.43.11 19686 0003e30cc50cd69210b8c36b-0ecf0120;1 1372466422;1\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						?assertMatch(
							#response{
								cookie = <<"24393_4">>,
								origin = null,
								type = reply,
								data = {{{_,_,_,_},_},{{_,_,_,_},_}}
							},
							ser_proto:decode(Answer)) end
			},
			{"Try to lookup non-existing session (it should create another one)",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"24393_4 Lc0,8,18,101 0003e30c-callid02@192.168.0.100 192.0.43.11 19686 0003e30cc50cd69210b8c36b-0ecf0120;1 1372466422;1\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						?assertMatch(
							#response{
								cookie = <<"24393_4">>,
								origin = null,
								type = reply,
								data = {{{_,_,_,_},_},{{_,_,_,_},_}}
							},
							ser_proto:decode(Answer)) end
			},
			{"Request brief statistics",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"356289 Ib\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						% Here is an error in the generic rtpproxy - it doesn't return cookie, e.g. does not prepend 356289 to the beginning
						?assertEqual(<<"356289 active sessions: 2\n">>, Answer) end
			},
			{"Request overall statistics",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"451309 I\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						% Here is an error in the generic rtpproxy - it doesn't return cookie, e.g. does not prepend 451309 to the beginning
						?assertEqual(<<"451309 sessions created: 2 active sessions: 2\n">>, Answer) end
			},
			{"Try to close existing session",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"24393_4 D 0003e30c-callid01@192.168.0.100 0003e30cc50cd69210b8c36b-0ecf0120 1372466422\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(
							#response{
								cookie = <<"24393_4">>,
								origin = null,
								type = reply,
								data = ok
							},
							ser_proto:decode(Answer)) end
			},
			{"Try to close non-existing session",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"24393_4 D NONEXISTENT-0003e30c-callid01@192.168.0.100 0003e30cc50cd69210b8c36b-0ecf0120 1372466422\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(
							#response{
								cookie = <<"24393_4">>,
								origin = null,
								type = error,
								data = notfound
							},
							ser_proto:decode(Answer)) end
			},
			{"Close all active sessions",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"198230 X\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"198230 0\n">>, Answer) end
			},
			{"Close all active sessions (again - must return ok)",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"198230 X\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"198230 0\n">>, Answer) end
			},

			%%
			%% Let's try close two sessions
			%%

			{"Try to create new session #1",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"24393_4 Uc0,8,18,101 0003e30c-callid01@192.168.0.100 192.0.43.10 27686 0003e30cc50cd69210b8c36b-0ecf0120;1\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						?assertMatch(
							#response{
								cookie = <<"24393_4">>,
								origin = null,
								type = reply,
								data = {{{_,_,_,_},_},{{_,_,_,_},_}}
							},
							ser_proto:decode(Answer)) end
			},
			{"Try to create new session #2",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"24393_4 Uc0,8,18,101 0003e30c-callid02@192.168.0.100 192.0.43.10 27686 0003e30cc50cd69210b8c36b-0ecf0120;1\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						?assertMatch(
							#response{
								cookie = <<"24393_4">>,
								origin = null,
								type = reply,
								data = {{{_,_,_,_},_},{{_,_,_,_},_}}
							},
							ser_proto:decode(Answer)) end
			},
			{"Request brief statistics",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"356289 Ib\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						% Here is an error in the generic rtpproxy - it doesn't return cookie, e.g. does not prepend 356289 to the beginning
						?assertEqual(<<"356289 active sessions: 2\n">>, Answer) end
			},
			{"Close all active sessions (again - must return ok)",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"198230 X\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"198230 0\n">>, Answer) end
			},
			{"Request brief statistics",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"356289 Ib\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer}} = gen_udp:recv(Fd, 0),
						% Here is an error in the generic rtpproxy - it doesn't return cookie, e.g. does not prepend 356289 to the beginning
						?assertEqual(<<"356289 active sessions: 0\n">>, Answer) end
			}
		]
	}.
