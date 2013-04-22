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

-module(rtpproxy_hold_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("rtplib/include/rtp.hrl").

-include("../src/common.hrl").

-define(RTPPROXY_IP, {127,0,0,1}).
-define(RTPPROXY_PORT, 33333).

rtpproxy_rtp_old_hold_via_invite_test_DISABLE() ->
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

	RtpProxyIpBinStr = list_to_binary(inet_parse:ntoa(?RTPPROXY_IP)),

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
			{"Try to create new session and send re-invite with 0.0.0.0",
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

					% Set session on hold
					gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<Cookie/binary, " Uc0,8,18,101 ", CallId/binary, " ", "0.0.0.0 ", SPort0/binary, " ", TagFrom/binary, ";1", " ", TagTo/binary, ";1", "\n">>),
					{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer2}} = gen_udp:recv(Fd, 0),

					BinRPort0 = list_to_binary(io_lib:format("~B", [RPort0])),
					?assertEqual(<<"24393_4 ", BinRPort0/binary, " 0.0.0.0\n">>, Answer2),

					% Other side is still able to receive data
					gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<Cookie/binary, " Lc0,8,18,101 ", CallId/binary, " ", "192.0.43.11 ", SPort1/binary, " ", TagFrom/binary, ";1", " ", TagTo/binary, ";1", "\n">>),
					{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer3}} = gen_udp:recv(Fd, 0),

					BinRPort1 = list_to_binary(io_lib:format("~B", [RPort1])),
					?assertEqual(<<"24393_4 ", BinRPort1/binary, " ", RtpProxyIpBinStr/binary, "\n">>, Answer3)
				end
			}
		]
	}.

rtpproxy_rtp_new_hold_test_() ->
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

	RtpProxyIpBinStr = list_to_binary(inet_parse:ntoa(?RTPPROXY_IP)),

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
			{"Try to create new session and put it on hold",
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

					% Put session on hold
					gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<Cookie/binary, " P20 ", CallId/binary, " ../test/default 0,8,18,101 ", TagFrom/binary, ";1", " ", TagTo/binary, ";1", "\n">>),
					{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer2}} = gen_udp:recv(Fd, 0),
					#response{cookie = <<"24393_4">>, data = ok} = ser_proto:decode(Answer2),

					% Other side starts receiving pre-recorded media
					{error, timeout} = gen_udp:recv(Fd0, 0, 500),
					{ok, {_, _, PlayerBin0}} = gen_udp:recv(Fd1, 0, 500),

					{ok, PlayerRtp0} = rtp:decode(PlayerBin0),
					?assertEqual({0, 1, 0, 0, <<0:1280>>}, {PlayerRtp0#rtp.padding, PlayerRtp0#rtp.marker, PlayerRtp0#rtp.payload_type, PlayerRtp0#rtp.sequence_number, PlayerRtp0#rtp.payload}),

					% Restore normal behaviour
					gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<Cookie/binary, " S ", CallId/binary, " ", TagFrom/binary, ";1", " ", TagTo/binary, ";1", "\n">>),
					{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, Answer3}} = gen_udp:recv(Fd, 0),
					#response{cookie = <<"24393_4">>, data = ok} = ser_proto:decode(Answer3),

					% Fetch all Player packets
					flush_fd(Fd1),

					% Continue exchanging data
					RtpHello2 = #rtp{marker = 0, payload_type = 0, sequence_number = 2, timestamp = 100, ssrc = 123456, payload = <<"hello from Alice">>},
					RtpHello3 = #rtp{marker = 0, payload_type = 0, sequence_number = 2, timestamp = 100, ssrc = 654321, payload = <<"hello from Bob">>},

					gen_udp:send(Fd0, I, RPort0, rtp:encode(RtpHello2)),
					gen_udp:send(Fd1, I, RPort1, rtp:encode(RtpHello3)),

					{ok, {_, _, RtpHello2Bin}} = gen_udp:recv(Fd1, 0, 500),
					{ok, {_, _, RtpHello3Bin}} = gen_udp:recv(Fd0, 0, 500),

					?assertEqual({{ok, RtpHello2}, {ok, RtpHello3}}, {rtp:decode(RtpHello2Bin), rtp:decode(RtpHello3Bin)})
				end
			}
		]
	}.

flush_fd(Fd) ->
	flush_fd(Fd, 0).
flush_fd(Fd, Number) ->
	error_logger:info_msg("Flush FD: attempt #~b~n", [Number]),
	case gen_udp:recv(Fd, 0, 100) of
		{ok, _} -> flush_fd(Fd, Number+1);
		{error, _} -> ok
	end.
