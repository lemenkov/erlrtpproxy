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

-module(proxy_backend_test).

%%
%% Please, start generic rtpproxy at the 127.0.0.1:33333 first
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("../src/common.hrl").

run_proxy_test_() ->
	{ok, Fd} = gen_udp:open(0, [{active, false}, list]),
	{setup,
		fun() ->
				gen_server:cast(listener, stop),
				udp_listener:start([{127,0,0,1}, 22222]),
				gen_server:cast(backend, stop)
		end,
		fun (_) ->
				gen_server:cast(listener, stop),
				gen_server:cast(backend, stop),
				gen_udp:close(Fd)
		end,
		[
			{"Try to start RTPPROXY backend first",
				fun () -> ?assertMatch({ok, Pid}, proxy_backend:start({{127,0,0,1}, 33333})) end
			},
			{"Try to handshake (get magic number back -20040107)",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, 22222, "592_36821 V\n"),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual("592_36821 20040107\n", Answer) end
			},
			{"Request basic RTP proxy functionality (ver. 20040107)",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, 22222, "6721_89367 VF 20040107\n"),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual("6721_89367 1\n", Answer) end
			},
			{"Request support for multiple RTP streams and MOH (ver. 20050322)",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, 22222, "6721_89367 VF 20050322\n"),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual("6721_89367 1\n", Answer) end
			},
			{"Request support for extra parameter in the V command (ver. 20060704)",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, 22222, "6721_89367 VF 20060704\n"),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual("6721_89367 1\n", Answer) end
			},
			{"Request support for RTP re-packetization (ver. 20071116)",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, 22222, "6721_89367 VF 20071116\n"),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual("6721_89367 1\n", Answer) end
			},
			{"Request support for forking (copying) RTP stream (ver. 20071218)",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, 22222, "6721_89367 VF 20071218\n"),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual("6721_89367 1\n", Answer) end
			},
			{"Request support for RTP statistics querying (ver. 20080403)",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, 22222, "6721_89367 VF 20080403\n"),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual("6721_89367 1\n", Answer) end
			},
			{"Request support for setting codecs in the update/lookup command (ver. 20081102)",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, 22222, "6721_89367 VF 20081102\n"),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual("6721_89367 1\n", Answer) end
			},
			{"Request support for session timeout notifications (ver. 20081224)",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, 22222, "6721_89367 VF 20081224\n"),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual("6721_89367 1\n", Answer) end
			},
			{"Request support for automatic bridging (ver. 20090810)",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, 22222, "6721_89367 VF 20090810\n"),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual("6721_89367 1\n", Answer) end
			},
			{"Request for unsupported extensions",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, 22222, "6721_89367 VF 20111109\n"),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual("6721_89367 E1\n", Answer) end
			},
			{"Try to create new session",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, 22222, "24393_4 Uc0,8,18,101 0003e30c-c50c00f7-123e8bd9-542f2edf@192.168.0.100 192.0.43.10 27686 0003e30cc50cd69210b8c36b-0ecf0120;1\n"),
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
						gen_udp:send(Fd, {127,0,0,1}, 22222, "24393_4 Lc0,8,18,101 0003e30c-c50c00f7-123e8bd9-542f2edf@192.168.0.100 192.0.43.11 19686 0003e30cc50cd69210b8c36b-0ecf0120;1 1372466422;1\n"),
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
%			{"Request overall statistics",
%				fun () ->
%						gen_udp:send(Fd, {127,0,0,1}, 22222, "6721_89367 I\n"),
%						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0, 1000),
%						?assertEqual("6721_89367 sessions created: 0\nactive sessions: 0\nactive streams: 0\n", Answer) end
%			},
			{"Try to close existing session",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, 22222, "24393_4 D 0003e30c-c50c00f7-123e8bd9-542f2edf@192.168.0.100 0003e30cc50cd69210b8c36b-0ecf0120 1372466422\n"),
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
						gen_udp:send(Fd, {127,0,0,1}, 22222, "6721_89367 X\n"),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0, 1000),
						?assertEqual("6721_89367 0\n", Answer) end
			}
		]
	}.

-endif.
