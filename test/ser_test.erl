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

-module(ser_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/common.hrl").

-define(RTPPROXY_IP, {127,0,0,1}).
-define(RTPPROXY_PORT, 33333).

ser_udp_test_() ->

	%%
	%% This is the socket which will be used for sending messages
	%%

	{ok, Fd} = gen_udp:open(0, [{active, false}, binary]),

	{setup,
		fun() ->
				%%
				%% Set node name
				%%

				net_kernel:start(['erlrtpproxy_test@localhost', longnames]),

				%%
				%% Load fake rtpproxy_ctl module - we'll test it later
				%%

				meck:new(rtpproxy_ctl),
				meck:expect(rtpproxy_ctl,
					command,
					fun
						(#cmd{
								origin=#origin{pid = Pid},
								type = message_u,
								callid = <<"0003e30c-callid01@192.168.0.100">>
							} = Cmd) ->
							gen_server:cast(Pid, {reply, Cmd, {{{192,168,0,1}, 12000}, {{192,168,0,1}, 12001}}});
						(#cmd{
								origin=#origin{pid = Pid},
								type = message_u,
								callid = <<"0003e30c-callid02@192.168.0.100">>
							} = Cmd) ->
							gen_server:cast(Pid, {reply, Cmd, {{{192,168,0,1}, 12000}, {{192,168,0,1}, 12001}}});
						(#cmd{
								origin=#origin{pid = Pid},
								type = message_i,
								params = [brief]
							} = Cmd) ->
							{ok, {stats, 0}};
						(#cmd{
								origin=#origin{pid = Pid},
								type = message_i,
								params = []
							} = Cmd) ->
							{ok, {stats, 100, 0}};
						(#cmd{
								origin=#origin{pid = Pid},
								type = message_d,
								callid = <<"0003e30c-callid02@192.168.0.100">>
							} = Cmd) ->
							ok;
						(#cmd{
								origin=#origin{pid = Pid},
								type = message_x
							} = Cmd) ->
							ok;
						(#cmd{origin=#origin{pid = Pid}} = Cmd) ->
							error_logger:info_msg("UNKNOWN for meck: ~p~n", [Cmd]),
							exit(1)
					end
				),

				%%
				%% Set necessary options
				%% (normally we'll set them in the /etc/erlrtpproxy.config
				%%

				application:set_env(rtpproxy, listen, {udp, "127.0.0.1", ?RTPPROXY_PORT}),
				application:set_env(rtpproxy, backend, ser),

				%%
				%% Start real SER frontend
				%%

				ser_sup:start_link()
		end,
		fun (_) ->
				gen_udp:close(Fd),
				meck:unload(rtpproxy_ctl),
				net_kernel:stop()
		end,
		[
			{"Try to handshake (get magic number back -20040107)",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"592_36821 V\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"592_36821 20040107\n">>, Answer) end
			},
			{"Request basic RTP proxy functionality (ver. 20040107)",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"6721_50320 VF 20040107\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"6721_50320 1\n">>, Answer) end
			},
			{"Request support for multiple RTP streams and MOH (ver. 20050322)",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"6721_09219 VF 20050322\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"6721_09219 1\n">>, Answer) end
			},
			{"Request support for extra parameter in the V command (ver. 20060704)",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"6721_86921 VF 20060704\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"6721_86921 1\n">>, Answer) end
			},
			{"Request support for RTP re-packetization (ver. 20071116)",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"6721_19382 VF 20071116\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"6721_19382 1\n">>, Answer) end
			},
			{"Request support for forking (copying) RTP stream (ver. 20071218)",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"195345 VF 20071218\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"195345 1\n">>, Answer) end
			},
			{"Request support for RTP statistics querying (ver. 20080403)",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"782361 VF 20080403\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"782361 1\n">>, Answer) end
			},
			{"Request support for setting codecs in the update/lookup command (ver. 20081102)",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"456987 VF 20081102\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"456987 1\n">>, Answer) end
			},
			{"Request support for session timeout notifications (ver. 20081224)",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"145698 VF 20081224\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"145698 1\n">>, Answer) end
			},
			{"Request support for automatic bridging (ver. 20090810)",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"352743 VF 20090810\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"352743 1\n">>, Answer) end
			},
			{"Request for unsupported extensions",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"809210 VF 20111109\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						% A generic rtpproxy returns 0 here which looks wrong. Should be E1 really.
						?assertEqual(<<"809210 E1\n">>, Answer) end
			},
			{"Try to create new session",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"24393_4 Uc0,8,18,101 0003e30c-callid01@192.168.0.100 192.0.43.10 27686 0003e30cc50cd69210b8c36b-0ecf0120;1\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
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
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
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
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						% Here is an error in the generic rtpproxy - it doesn't return cookie, e.g. does not prepend 356289 to the beginning
						?assertEqual(<<"356289 active sessions: 0\n">>, Answer) end
			},
			{"Request overall statistics",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"451309 I\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						% Here is an error in the generic rtpproxy - it doesn't return cookie, e.g. does not prepend 451309 to the beginning
						?assertMatch(<<"451309 sessions created: 100 active sessions: 0\n">>, Answer) end
			},
			{"Try to close existing session",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"24393_4 D 0003e30c-callid02@192.168.0.100 0003e30cc50cd69210b8c36b-0ecf0120 1372466422\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(
							#response{
								cookie = <<"24393_4">>,
								origin = null,
								type = reply,
								data = ok
							},
							ser_proto:decode(Answer)) end
			},
			{"Close all active sessions",
				fun () ->
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<"198230 X\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"198230 0\n">>, Answer) end
			}
		]
	}.

ser_tcp_test_DISABLED() ->

	%%
	%% Set node name
	%%

	net_kernel:start(['erlrtpproxy_test@localhost', longnames]),

	%%
	%% Set necessary options
	%% (normally we'll set them in the /etc/erlrtpproxy.config
	%%

	application:set_env(rtpproxy, listen, {tcp, "127.0.0.1", ?RTPPROXY_PORT}),
	application:set_env(rtpproxy, backend, ser),

	%%
	%% Start real SER frontend
	%%

	ser_sup:start_link(),

	%%
	%% This is the socket which will be used for sending messages
	%%

	{ok, Fd} = gen_tcp:connect(?RTPPROXY_IP, ?RTPPROXY_PORT, [{active, false}, binary]),

	{setup,
		fun() ->
				%%
				%% Load fake rtpproxy_ctl module - we'll test it later
				%%

				meck:new(rtpproxy_ctl),
				meck:expect(rtpproxy_ctl,
					command,
					fun
						(#cmd{
								origin=#origin{pid = Pid},
								type = message_u,
								callid = <<"0003e30c-callid01@192.168.0.100">>
							} = Cmd) ->
							gen_server:cast(Pid, {reply, Cmd, {{{192,168,0,1}, 12000}, {{192,168,0,1}, 12001}}});
						(#cmd{
								origin=#origin{pid = Pid},
								type = message_u,
								callid = <<"0003e30c-callid02@192.168.0.100">>
							} = Cmd) ->
							gen_server:cast(Pid, {reply, Cmd, {{{192,168,0,1}, 12000}, {{192,168,0,1}, 12001}}});
						(#cmd{
								origin=#origin{pid = Pid},
								type = message_i,
								params = [brief]
							} = Cmd) ->
							{ok, {stats, 0}};
						(#cmd{
								origin=#origin{pid = Pid},
								type = message_i,
								params = []
							} = Cmd) ->
							{ok, {stats, 100, 0}};
						(#cmd{
								origin=#origin{pid = Pid},
								type = message_d,
								callid = <<"0003e30c-callid02@192.168.0.100">>
							} = Cmd) ->
							ok;
						(#cmd{
								origin=#origin{pid = Pid},
								type = message_x
							} = Cmd) ->
							ok;
						(#cmd{origin=#origin{pid = Pid}} = Cmd) ->
							error_logger:info_msg("UNKNOWN for meck: ~p~n", [Cmd]),
							exit(1)
					end
				)
		end,
		fun (_) ->
				meck:unload(rtpproxy_ctl),
				gen_tcp:close(Fd),
				net_kernel:stop()
		end,
		[
			{"Try to handshake (get magic number back -20040107)",
				fun () ->
						gen_tcp:send(Fd, <<"592_36821 V\n">>),
						{ok, Answer} = gen_tcp:recv(Fd, 0),
						?assertEqual(<<"592_36821 20040107\n">>, Answer) end
			},
			{"Request basic RTP proxy functionality (ver. 20040107)",
				fun () ->
						gen_tcp:send(Fd, <<"6721_50320 VF 20040107\n">>),
						{ok, Answer} = gen_tcp:recv(Fd, 0),
						?assertEqual(<<"6721_50320 1\n">>, Answer) end
			},
			{"Request support for multiple RTP streams and MOH (ver. 20050322)",
				fun () ->
						gen_tcp:send(Fd, <<"6721_09219 VF 20050322\n">>),
						{ok, Answer} = gen_tcp:recv(Fd, 0),
						?assertEqual(<<"6721_09219 1\n">>, Answer) end
			},
			{"Request support for extra parameter in the V command (ver. 20060704)",
				fun () ->
						gen_tcp:send(Fd, <<"6721_86921 VF 20060704\n">>),
						{ok, Answer} = gen_tcp:recv(Fd, 0),
						?assertEqual(<<"6721_86921 1\n">>, Answer) end
			},
			{"Request support for RTP re-packetization (ver. 20071116)",
				fun () ->
						gen_tcp:send(Fd, <<"6721_19382 VF 20071116\n">>),
						{ok, Answer} = gen_tcp:recv(Fd, 0),
						?assertEqual(<<"6721_19382 1\n">>, Answer) end
			},
			{"Request support for forking (copying) RTP stream (ver. 20071218)",
				fun () ->
						gen_tcp:send(Fd, <<"195345 VF 20071218\n">>),
						{ok, Answer} = gen_tcp:recv(Fd, 0),
						?assertEqual(<<"195345 1\n">>, Answer) end
			},
			{"Request support for RTP statistics querying (ver. 20080403)",
				fun () ->
						gen_tcp:send(Fd, <<"782361 VF 20080403\n">>),
						{ok, Answer} = gen_tcp:recv(Fd, 0),
						?assertEqual(<<"782361 1\n">>, Answer) end
			},
			{"Request support for setting codecs in the update/lookup command (ver. 20081102)",
				fun () ->
						gen_tcp:send(Fd, <<"456987 VF 20081102\n">>),
						{ok, Answer} = gen_tcp:recv(Fd, 0),
						?assertEqual(<<"456987 1\n">>, Answer) end
			},
			{"Request support for session timeout notifications (ver. 20081224)",
				fun () ->
						gen_tcp:send(Fd, <<"145698 VF 20081224\n">>),
						{ok, Answer} = gen_tcp:recv(Fd, 0),
						?assertEqual(<<"145698 1\n">>, Answer) end
			},
			{"Request support for automatic bridging (ver. 20090810)",
				fun () ->
						gen_tcp:send(Fd, <<"352743 VF 20090810\n">>),
						{ok, Answer} = gen_tcp:recv(Fd, 0),
						?assertEqual(<<"352743 1\n">>, Answer) end
			},
			{"Request for unsupported extensions",
				fun () ->
						gen_tcp:send(Fd, <<"809210 VF 20111109\n">>),
						{ok, Answer} = gen_tcp:recv(Fd, 0),
						% A generic rtpproxy returns 0 here which looks wrong. Should be E1 really.
						?assertEqual(<<"809210 E1\n">>, Answer) end
			},
			{"Try to create new session",
				fun () ->
						gen_tcp:send(Fd, <<"24393_4 Uc0,8,18,101 0003e30c-callid01@192.168.0.100 192.0.43.10 27686 0003e30cc50cd69210b8c36b-0ecf0120;1\n">>),
						{ok, Answer} = gen_tcp:recv(Fd, 0),
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
						gen_tcp:send(Fd, <<"24393_4 Lc0,8,18,101 0003e30c-callid01@192.168.0.100 192.0.43.11 19686 0003e30cc50cd69210b8c36b-0ecf0120;1 1372466422;1\n">>),
						{ok, Answer} = gen_tcp:recv(Fd, 0),
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
						gen_tcp:send(Fd, <<"356289 Ib\n">>),
						{ok, Answer} = gen_tcp:recv(Fd, 0),
						% Here is an error in the generic rtpproxy - it doesn't return cookie, e.g. does not prepend 356289 to the beginning
						?assertMatch(<<"356289 active sessions: ", _/binary>>, Answer) end
			},
			{"Request overall statistics",
				fun () ->
						gen_tcp:send(Fd, <<"451309 I\n">>),
						{ok, Answer} = gen_tcp:recv(Fd, 0),
						% Here is an error in the generic rtpproxy - it doesn't return cookie, e.g. does not prepend 451309 to the beginning
						?assertMatch(<<"451309 sessions created: ", _/binary>>, Answer) end
			},
			{"Try to close existing session",
				fun () ->
						gen_tcp:send(Fd, <<"24393_4 D 0003e30c-callid02@192.168.0.100 0003e30cc50cd69210b8c36b-0ecf0120 1372466422\n">>),
						{ok, Answer} = gen_tcp:recv(Fd, 0),
						?assertEqual(
							#response{
								cookie = <<"24393_4">>,
								origin = null,
								type = reply,
								data = ok
							},
							ser_proto:decode(Answer)) end
			},
			{"Close all active sessions",
				fun () ->
						gen_tcp:send(Fd, <<"198230 X\n">>),
						{ok, Answer} = gen_tcp:recv(Fd, 0),
						?assertEqual(<<"198230 0\n">>, Answer) end
			}
		]
	}.
