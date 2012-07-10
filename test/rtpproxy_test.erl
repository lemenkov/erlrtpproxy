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

-module(rtpproxy_test).

%%
%% Please, start generic rtpproxy at the 127.0.0.1:33333 first
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("../src/common.hrl").

-define(RTPPROXY_PORT, 33333).

run_proxy_test_() ->
	{ok, Fd} = gen_udp:open(0, [{active, false}, binary]),
	{setup,
		fun() -> true end,
		fun (_) -> gen_udp:close(Fd) end,
		[
			{"Try to handshake (get magic number back -20040107)",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, ?RTPPROXY_PORT, <<"592_36821 V\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"592_36821 20040107\n">>, Answer) end
			},
			{"Request basic RTP proxy functionality (ver. 20040107)",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, ?RTPPROXY_PORT, <<"6721_50320 VF 20040107\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"6721_50320 1\n">>, Answer) end
			},
			{"Request support for multiple RTP streams and MOH (ver. 20050322)",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, ?RTPPROXY_PORT, <<"6721_09219 VF 20050322\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"6721_09219 1\n">>, Answer) end
			},
			{"Request support for extra parameter in the V command (ver. 20060704)",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, ?RTPPROXY_PORT, <<"6721_86921 VF 20060704\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"6721_86921 1\n">>, Answer) end
			},
			{"Request support for RTP re-packetization (ver. 20071116)",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, ?RTPPROXY_PORT, <<"6721_19382 VF 20071116\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"6721_19382 1\n">>, Answer) end
			},
			{"Request support for forking (copying) RTP stream (ver. 20071218)",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, ?RTPPROXY_PORT, <<"195345 VF 20071218\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"195345 1\n">>, Answer) end
			},
			{"Request support for RTP statistics querying (ver. 20080403)",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, ?RTPPROXY_PORT, <<"782361 VF 20080403\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"782361 1\n">>, Answer) end
			},
			{"Request support for setting codecs in the update/lookup command (ver. 20081102)",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, ?RTPPROXY_PORT, <<"456987 VF 20081102\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"456987 1\n">>, Answer) end
			},
			% FIXME
			{"Request support for session timeout notifications (ver. 20081224 - not supported by this proxy)",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, ?RTPPROXY_PORT, <<"145698 VF 20081224\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"145698 0\n">>, Answer) end
			},
			% FIXME
			{"Request support for automatic bridging (ver. 20090810 - not supported by this proxy)",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, ?RTPPROXY_PORT, <<"352743 VF 20090810\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"352743 0\n">>, Answer) end
			},
			% FIXME should be E1
			{"Request for unsupported extensions",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, ?RTPPROXY_PORT, <<"809210 VF 20111109\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0),
						?assertEqual(<<"809210 0\n">>, Answer) end
			},
			{"Try to create new session",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, ?RTPPROXY_PORT, <<"24393_4 Uc0,8,18,101 0003e30c-callid01@192.168.0.100 192.0.43.10 27686 0003e30cc50cd69210b8c36b-0ecf0120;1\n">>),
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
						gen_udp:send(Fd, {127,0,0,1}, ?RTPPROXY_PORT, <<"24393_4 Lc0,8,18,101 0003e30c-callid01@192.168.0.100 192.0.43.11 19686 0003e30cc50cd69210b8c36b-0ecf0120;1 1372466422;1\n">>),
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
						gen_udp:send(Fd, {127,0,0,1}, ?RTPPROXY_PORT, <<"356289 Ib\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0, 1000),
						% Here is an error in rtpproxy - it should return cookie as well, e.g. prepend 356289 to the beginning
						% ?assertMatch(<<"356289 sessions created: ", _/binary>>, Answer) end
						?assertMatch(<<" active sessions: ", _/binary>>, Answer) end
			},
			{"Request overall statistics",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, ?RTPPROXY_PORT, <<"451309 I\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0, 1000),
						% Here is an error in rtpproxy - it should return cookie as well, e.g. prepend 451309 to the beginning
						% ?assertMatch(<<"451309 sessions created: ", _/binary>>, Answer) end
						?assertMatch(<<" sessions created: ", _/binary>>, Answer) end
			},
			{"Try to close existing session",
				fun () ->
						gen_udp:send(Fd, {127,0,0,1}, ?RTPPROXY_PORT, <<"24393_4 D 0003e30c-callid02@192.168.0.100 0003e30cc50cd69210b8c36b-0ecf0120 1372466422\n">>),
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
						gen_udp:send(Fd, {127,0,0,1}, ?RTPPROXY_PORT, <<"198230 X\n">>),
						{ok, {Ip, Port, Answer}} = gen_udp:recv(Fd, 0, 1000),
						?assertEqual(<<"198230 0\n">>, Answer) end
			}
		]
	}.

-endif.
