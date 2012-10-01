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

-module(rtpproxy_rtp_av_handling_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("rtplib/include/rtp.hrl").

-include("../include/common.hrl").

-define(RTPPROXY_IP, {127,0,0,1}).
-define(RTPPROXY_PORT, 33333).

rtpproxy_rtp_handling_test_() ->
	%%
	%% This is the socket which will be used for sending commands and receiving notifications messages
	%%

	{ok, Fd} = gen_udp:open(0, [{active, false}, binary]),

	Cookie = <<"24393_4">>,
	CallId = <<"0003e30c-callid01@192.168.0.100">>,
	TagFrom = <<"0003e30cc50cd69210b8c36b-0ecf0120">>,
	TagTo = <<"1372466422">>,

	%%
	%% These are the sockets which will be used for sending/receiving RTP audio
	%%

	{ok, Fd0} = gen_udp:open(0, [{active, false}, binary]),
	{ok, {_, Port0}} = inet:sockname(Fd0),

	{ok, Fd1} = gen_udp:open(0, [{active, false}, binary]),
	{ok, {_, Port1}} = inet:sockname(Fd1),

	APort0 = list_to_binary(io_lib:format("~b", [Port0])),
	APort1 = list_to_binary(io_lib:format("~b", [Port1])),

	%%
	%% These are the sockets which will be used for sending/receiving RTP video
	%%

	{ok, Fd2} = gen_udp:open(0, [{active, false}, binary]),
	{ok, {_, Port2}} = inet:sockname(Fd2),

	{ok, Fd3} = gen_udp:open(0, [{active, false}, binary]),
	{ok, {_, Port3}} = inet:sockname(Fd3),

	VPort0 = list_to_binary(io_lib:format("~b", [Port2])),
	VPort1 = list_to_binary(io_lib:format("~b", [Port3])),

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
				% Close session
				gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<Cookie/binary, " D ", CallId/binary, " ", TagFrom/binary, " ", TagTo/binary, "\n">>),
				{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, _}} = gen_udp:recv(Fd, 0),

				gen_udp:close(Fd),

				gen_udp:close(Fd0),
				gen_udp:close(Fd1),
				gen_udp:close(Fd2),
				gen_udp:close(Fd3),

				application:stop(rtpproxy),
				application:stop(gproc),
				gen_server:cast({global, rtpproxy_notifier}, stop),
				pool:stop(),
				net_kernel:stop()
		end,
		[
			{"Try to create new session with two media streams (A/V) and exchange RTP packets (with IP:Port and SSRC checking)",
				fun () ->
						%%
						%% Create sessions
						%%

						% Create session for Audio
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<Cookie/binary, " Uc0,8,18,101 ", CallId/binary, " ", "192.0.43.10 ", APort0/binary, " ", TagFrom/binary, ";1", "\n">>),

						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, AAnswer0}} = gen_udp:recv(Fd, 0),
						#response{cookie = <<"24393_4">>, data = {{{_,_,_,_}=I,ARPort0},{{_,_,_,_},_}}} = ser_proto:decode(AAnswer0),

						% Create session for Video
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<Cookie/binary, " Uc0,8,18,101 ", CallId/binary, " ", "192.0.43.10 ", VPort0/binary, " ", TagFrom/binary, ";2", "\n">>),

						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, VAnswer0}} = gen_udp:recv(Fd, 0),
						#response{cookie = <<"24393_4">>, data = {{{_,_,_,_}=I,VRPort0},{{_,_,_,_},_}}} = ser_proto:decode(VAnswer0),

						%%
						%% Lookup sessions
						%%

						% Lookup session for Audio
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<Cookie/binary, " Lc0,8,18,101 ", CallId/binary, " ", "192.0.43.11 ", APort1/binary, " ", TagFrom/binary, ";1", " ", TagTo/binary, ";1", "\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, AAnswer1}} = gen_udp:recv(Fd, 0),
						#response{cookie = <<"24393_4">>, data = {{{_,_,_,_}=I,ARPort1},{{_,_,_,_},_}}} = ser_proto:decode(AAnswer1),

						% Lookup session for Video
						gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<Cookie/binary, " Lc0,8,18,101 ", CallId/binary, " ", "192.0.43.11 ", VPort1/binary, " ", TagFrom/binary, ";2", " ", TagTo/binary, ";2", "\n">>),
						{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, VAnswer1}} = gen_udp:recv(Fd, 0),
						#response{cookie = <<"24393_4">>, data = {{{_,_,_,_}=I,VRPort1},{{_,_,_,_},_}}} = ser_proto:decode(VAnswer1),

						%%
						%% Create test RTP
						%%

						RtpHello0 = #rtp{marker = 1, payload_type = 0, sequence_number = 1, timestamp = 100, ssrc = 123456, payload = <<"hello from Alice">>},
						RtpHello1 = #rtp{marker = 1, payload_type = 0, sequence_number = 1, timestamp = 100, ssrc = 654321, payload = <<"hello from Bob">>},

						%%
						%% Pair Audio
						%%

						gen_udp:send(Fd0, I, ARPort0, rtp:encode(RtpHello0)),
						gen_udp:send(Fd1, I, ARPort1, rtp:encode(RtpHello1)),

						gen_udp:recv(Fd0, 0, 500),
						gen_udp:recv(Fd1, 0, 500),

						%%
						%% Pair Video
						%%

						gen_udp:send(Fd2, I, VRPort0, rtp:encode(RtpHello0)),
						gen_udp:send(Fd3, I, VRPort1, rtp:encode(RtpHello1)),

						gen_udp:recv(Fd2, 0, 500),
						gen_udp:recv(Fd3, 0, 500),

						%%
						%% Now we have a connected ports
						%%

						RtpHello00 = #rtp{payload_type = 0, sequence_number = 2, timestamp = 200, ssrc = 123456, payload = <<"hello from Alice">>},
						RtpHello11 = #rtp{payload_type = 0, sequence_number = 2, timestamp = 200, ssrc = 654321, payload = <<"hello from Bob">>},

						gen_udp:send(Fd0, I, ARPort0, rtp:encode(RtpHello00)),
						gen_udp:send(Fd1, I, ARPort1, rtp:encode(RtpHello11)),

						{ok, {I, ARPort0, Bin0}} = gen_udp:recv(Fd0, 0, 500),
						{ok, {I, ARPort1, Bin1}} = gen_udp:recv(Fd1, 0, 500),

						gen_udp:send(Fd2, I, VRPort0, rtp:encode(RtpHello00)),
						gen_udp:send(Fd3, I, VRPort1, rtp:encode(RtpHello11)),

						{ok, {I, VRPort0, Bin0}} = gen_udp:recv(Fd2, 0, 500),
						{ok, {I, VRPort1, Bin1}} = gen_udp:recv(Fd3, 0, 500),

						% We've got the message from the other side - decode and compare
						?assertEqual({{ok, RtpHello11}, {ok, RtpHello00}}, {rtp:decode(Bin0), rtp:decode(Bin1)})
				end
			}
		]
	}.
