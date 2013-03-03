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

-module(rtpproxy_rtp_handling_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("rtplib/include/rtp.hrl").

-include("../include/common.hrl").

-define(RTPPROXY_IP, {127,0,0,1}).
-define(RTPPROXY_PORT, 33333).

rtpproxy_rtp_handling_active_once_test_() ->
	%%
	%% This is the socket which will be used for sending commands and receiving notifications messages
	%%

	{ok, Fd} = gen_udp:open(0, [{active, false}, binary]),

	%%
	%% These are the sockets which will be used for sending/receiving RTP
	%%

	{ok, Fd0} = gen_udp:open(0, [{active, false}, binary]),
	{ok, {_, Port0}} = inet:sockname(Fd0),

	{ok, Fd1} = gen_udp:open(0, [{active, false}, binary]),
	{ok, {_, Port1}} = inet:sockname(Fd1),

	Cookie = <<"24393_4">>,
	CallId = <<"0003e30c-callid01@192.168.0.100">>,
	TagFrom = <<"0003e30cc50cd69210b8c36b-0ecf0120">>,
	TagTo = <<"1372466422">>,

	SPort0 = list_to_binary(io_lib:format("~b", [Port0])),
	SPort1 = list_to_binary(io_lib:format("~b", [Port1])),

	{setup,
		fun() ->
				%%
				%% Set node name
				%%

				net_kernel:start(['erlrtpproxy_payload_test@localhost', longnames]),

				%%
				%% Set necessary options
				%% (normally we'll set them in the /etc/erlrtpproxy.config
				%%

				test_utils:set_default_opts(),

				%%
				%% Start rtpproxy
				%%

				test_utils:start()
		end,
		fun (_) ->
				% Close session
				gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<Cookie/binary, " D ", CallId/binary, " ", TagFrom/binary, " ", TagTo/binary, "\n">>),
				{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, _}} = gen_udp:recv(Fd, 0),

				gen_udp:close(Fd),

				%%
				%% Stop rtpproxy
				%%

				test_utils:stop()
		end,
		[
			{"Try to create new session and exchange RTP packets (with IP:Port and SSRC checking)",
				fun () ->
						% Create session
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<Cookie/binary, " Uc0,8,18,101 ", CallId/binary, " ", "192.0.43.10 ", SPort0/binary, " ", TagFrom/binary, ";1", "\n">>),

						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer0}} = gen_udp:recv(Fd, 0),
						#response{cookie = <<"24393_4">>, data = {{{_,_,_,_}=I,RPort0},{{_,_,_,_},_}}} = ser_proto:decode(Answer0),


						% Lookup session
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<Cookie/binary, " Lc0,8,18,101 ", CallId/binary, " ", "192.0.43.11 ", SPort1/binary, " ", TagFrom/binary, ";1", " ", TagTo/binary, ";1", "\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer1}} = gen_udp:recv(Fd, 0),
						#response{cookie = <<"24393_4">>, data = {{{_,_,_,_}=I,RPort1},{{_,_,_,_},_}}} = ser_proto:decode(Answer1),

						RtpHello0 = #rtp{marker = 1, payload_type = 0, sequence_number = 1, timestamp = 100, ssrc = 123456, payload = <<"hello from Alice">>},
						RtpHello1 = #rtp{marker = 1, payload_type = 0, sequence_number = 1, timestamp = 100, ssrc = 654321, payload = <<"hello from Bob">>},

						gen_udp:send(Fd0, I, RPort0, rtp:encode(RtpHello0)),
						gen_udp:send(Fd1, I, RPort1, rtp:encode(RtpHello1)),

						gen_udp:recv(Fd0, 0, 500),
						gen_udp:recv(Fd1, 0, 500),

						% Now we have a connected ports

						RtpHello00 = #rtp{payload_type = 0, sequence_number = 2, timestamp = 200, ssrc = 123456, payload = <<"hello from Alice">>},
						RtpHello11 = #rtp{payload_type = 0, sequence_number = 2, timestamp = 200, ssrc = 654321, payload = <<"hello from Bob">>},

						gen_udp:send(Fd0, I, RPort0, rtp:encode(RtpHello00)),
						gen_udp:send(Fd1, I, RPort1, rtp:encode(RtpHello11)),

						{ok, {I, RPort0, Bin0}} = gen_udp:recv(Fd0, 0, 500),
						{ok, {I, RPort1, Bin1}} = gen_udp:recv(Fd1, 0, 500),

						% We've got the message from the other side - decode and compare
						?assertEqual({{ok, RtpHello11}, {ok, RtpHello00}}, {rtp:decode(Bin0), rtp:decode(Bin1)})
				end
			}
		]
	}.

rtpproxy_rtp_handling_active_true_test_() ->
	%%
	%% This is the socket which will be used for sending commands and receiving notifications messages
	%%

	{ok, Fd} = gen_udp:open(0, [{active, false}, binary]),

	%%
	%% These are the sockets which will be used for sending/receiving RTP
	%%

	{ok, Fd0} = gen_udp:open(0, [{active, false}, binary]),
	{ok, {_, Port0}} = inet:sockname(Fd0),

	{ok, Fd1} = gen_udp:open(0, [{active, false}, binary]),
	{ok, {_, Port1}} = inet:sockname(Fd1),

	Cookie = <<"24393_4">>,
	CallId = <<"0003e30c-callid01@192.168.0.100">>,
	TagFrom = <<"0003e30cc50cd69210b8c36b-0ecf0120">>,
	TagTo = <<"1372466422">>,

	SPort0 = list_to_binary(io_lib:format("~b", [Port0])),
	SPort1 = list_to_binary(io_lib:format("~b", [Port1])),

	{setup,
		fun() ->
				%%
				%% Set node name
				%%

				net_kernel:start(['erlrtpproxy_payload_test@localhost', longnames]),

				%%
				%% Set necessary options
				%% (normally we'll set them in the /etc/erlrtpproxy.config
				%%

				test_utils:set_default_opts(),
				application:set_env(rtpproxy, active, true),

				%%
				%% Start rtpproxy
				%%

				test_utils:start()
		end,
		fun (_) ->
				% Close session
				gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<Cookie/binary, " D ", CallId/binary, " ", TagFrom/binary, " ", TagTo/binary, "\n">>),
				{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, _}} = gen_udp:recv(Fd, 0),

				gen_udp:close(Fd),

				%%
				%% Stop rtpproxy
				%%

				test_utils:stop()
		end,
		[
			{"Try to create new session and exchange RTP packets (with IP:Port and SSRC checking)",
				fun () ->
						% Create session
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<Cookie/binary, " Uc0,8,18,101 ", CallId/binary, " ", "192.0.43.10 ", SPort0/binary, " ", TagFrom/binary, ";1", "\n">>),

						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer0}} = gen_udp:recv(Fd, 0),
						#response{cookie = <<"24393_4">>, data = {{{_,_,_,_}=I,RPort0},{{_,_,_,_},_}}} = ser_proto:decode(Answer0),


						% Lookup session
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<Cookie/binary, " Lc0,8,18,101 ", CallId/binary, " ", "192.0.43.11 ", SPort1/binary, " ", TagFrom/binary, ";1", " ", TagTo/binary, ";1", "\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer1}} = gen_udp:recv(Fd, 0),
						#response{cookie = <<"24393_4">>, data = {{{_,_,_,_}=I,RPort1},{{_,_,_,_},_}}} = ser_proto:decode(Answer1),

						RtpHello0 = #rtp{marker = 1, payload_type = 0, sequence_number = 1, timestamp = 100, ssrc = 123456, payload = <<"hello from Alice">>},
						RtpHello1 = #rtp{marker = 1, payload_type = 0, sequence_number = 1, timestamp = 100, ssrc = 654321, payload = <<"hello from Bob">>},

						gen_udp:send(Fd0, I, RPort0, rtp:encode(RtpHello0)),
						gen_udp:send(Fd1, I, RPort1, rtp:encode(RtpHello1)),

						gen_udp:recv(Fd0, 0, 500),
						gen_udp:recv(Fd1, 0, 500),

						% Now we have a connected ports

						RtpHello00 = #rtp{payload_type = 0, sequence_number = 2, timestamp = 200, ssrc = 123456, payload = <<"hello from Alice">>},
						RtpHello11 = #rtp{payload_type = 0, sequence_number = 2, timestamp = 200, ssrc = 654321, payload = <<"hello from Bob">>},

						gen_udp:send(Fd0, I, RPort0, rtp:encode(RtpHello00)),
						gen_udp:send(Fd1, I, RPort1, rtp:encode(RtpHello11)),

						{ok, {I, RPort0, Bin0}} = gen_udp:recv(Fd0, 0, 500),
						{ok, {I, RPort1, Bin1}} = gen_udp:recv(Fd1, 0, 500),

						% We've got the message from the other side - decode and compare
						?assertEqual({{ok, RtpHello11}, {ok, RtpHello00}}, {rtp:decode(Bin0), rtp:decode(Bin1)})
				end
			}
		]
	}.

rtpproxy_rtp_handling_rebuildrtp_true_test_() ->
	%%
	%% This is the socket which will be used for sending commands and receiving notifications messages
	%%

	{ok, Fd} = gen_udp:open(0, [{active, false}, binary]),

	%%
	%% These are the sockets which will be used for sending/receiving RTP
	%%

	{ok, Fd0} = gen_udp:open(0, [{active, false}, binary]),
	{ok, {_, Port0}} = inet:sockname(Fd0),

	{ok, Fd1} = gen_udp:open(0, [{active, false}, binary]),
	{ok, {_, Port1}} = inet:sockname(Fd1),

	Cookie = <<"24393_4">>,
	CallId = <<"0003e30c-callid01@192.168.0.100">>,
	TagFrom = <<"0003e30cc50cd69210b8c36b-0ecf0120">>,
	TagTo = <<"1372466422">>,

	SPort0 = list_to_binary(io_lib:format("~b", [Port0])),
	SPort1 = list_to_binary(io_lib:format("~b", [Port1])),

	{setup,
		fun() ->
				%%
				%% Set node name
				%%

				net_kernel:start(['erlrtpproxy_payload_test@localhost', longnames]),

				%%
				%% Set necessary options
				%% (normally we'll set them in the /etc/erlrtpproxy.config
				%%

				test_utils:set_default_opts(),
				application:set_env(rtpproxy, rebuildrtp, true),

				%%
				%% Start rtpproxy
				%%

				test_utils:start()
		end,
		fun (_) ->
				% Close session
				gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<Cookie/binary, " D ", CallId/binary, " ", TagFrom/binary, " ", TagTo/binary, "\n">>),
				{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, _}} = gen_udp:recv(Fd, 0),

				gen_udp:close(Fd),

				%%
				%% Stop rtpproxy
				%%

				test_utils:stop()
		end,
		[
			{"Try to create new session and exchange RTP packets (with IP:Port and SSRC checking)",
				fun () ->
						% Create session
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<Cookie/binary, " Uc0,8,18,101 ", CallId/binary, " ", "192.0.43.10 ", SPort0/binary, " ", TagFrom/binary, ";1", "\n">>),

						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer0}} = gen_udp:recv(Fd, 0),
						#response{cookie = <<"24393_4">>, data = {{{_,_,_,_}=I,RPort0},{{_,_,_,_},_}}} = ser_proto:decode(Answer0),


						% Lookup session
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<Cookie/binary, " Lc0,8,18,101 ", CallId/binary, " ", "192.0.43.11 ", SPort1/binary, " ", TagFrom/binary, ";1", " ", TagTo/binary, ";1", "\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer1}} = gen_udp:recv(Fd, 0),
						#response{cookie = <<"24393_4">>, data = {{{_,_,_,_}=I,RPort1},{{_,_,_,_},_}}} = ser_proto:decode(Answer1),

						RtpHello0 = #rtp{marker = 1, payload_type = 0, sequence_number = 1, timestamp = 100, ssrc = 123456, payload = <<"hello from Alice">>},
						RtpHello1 = #rtp{marker = 1, payload_type = 0, sequence_number = 1, timestamp = 100, ssrc = 654321, payload = <<"hello from Bob">>},

						gen_udp:send(Fd0, I, RPort0, rtp:encode(RtpHello0)),
						gen_udp:send(Fd1, I, RPort1, rtp:encode(RtpHello1)),

						gen_udp:recv(Fd0, 0, 500),
						gen_udp:recv(Fd1, 0, 500),

						% Now we have a connected ports

						RtpHello00 = #rtp{payload_type = 0, sequence_number = 2, timestamp = 200, ssrc = 123456, payload = <<"hello from Alice">>},
						RtpHello11 = #rtp{payload_type = 0, sequence_number = 2, timestamp = 200, ssrc = 654321, payload = <<"hello from Bob">>},

						gen_udp:send(Fd0, I, RPort0, rtp:encode(RtpHello00)),
						gen_udp:send(Fd1, I, RPort1, rtp:encode(RtpHello11)),

						{ok, {I, RPort0, Bin0}} = gen_udp:recv(Fd0, 0, 500),
						{ok, {I, RPort1, Bin1}} = gen_udp:recv(Fd1, 0, 500),

						{ok, RtpHelloRet11} = rtp:decode(Bin0),
						{ok, RtpHelloRet00} = rtp:decode(Bin1),

						% We've got the message from the other side - decode and compare
						?assertMatch(
							{
								#rtp{payload_type = 0, sequence_number = 2, timestamp = 200, payload = <<"hello from Alice">>},
								#rtp{payload_type = 0, sequence_number = 2, timestamp = 200, payload = <<"hello from Bob">>}
							},
							{RtpHelloRet00, RtpHelloRet11}
						)
				end
			}
		]
	}.
