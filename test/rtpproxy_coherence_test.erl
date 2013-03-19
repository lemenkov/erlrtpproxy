%%%----------------------------------------------------------------------
%%% Copyright (c) 2013 Peter Lemenkov <lemenkov@gmail.com>
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

-module(rtpproxy_coherence_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("rtplib/include/rtp.hrl").

-include("../include/common.hrl").

-define(RTPPROXY_IP, {127,0,0,1}).
-define(RTPPROXY_PORT, 33333).

% Create a call and wait for timeout
rtpproxy_coherence_test_() ->
	%%
	%% This is the socket which will be used for sending commands and receiving notifications messages
	%%

	{ok, Fd} = gen_udp:open(0, [{active, false}, binary]),

	Cookie = <<"24393_4">>,
	CallId = <<"0003e30c-callid01@192.168.0.100">>,
	TagFrom = <<"0003e30cc50cd69210b8c36b-0ecf0120">>,
	TagTo = <<"1372466422">>,


	{setup,
		fun() ->
				%%
				%% Set node name
				%%

				net_kernel:start(['erlrtpproxy_coherence_test@localhost', longnames]),

				%%
				%% Set necessary options
				%% (normally we'll set them in the /etc/erlrtpproxy.config
				%%

				test_utils:set_default_opts(),
				application:set_env(rtpproxy, ttl, 1),
				application:set_env(rtpproxy, ttl_early, 1),

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

		% Increase the default timeout
		{timeout, 60,
			[
				{"Try to create new session and wait for timeout",
					fun () ->
							% Create session
							gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<Cookie/binary, " Uc0,8,18,101 ", CallId/binary, " ", "192.0.43.10 12345 ", TagFrom/binary, ";1", "\n">>),
							{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, _}} = gen_udp:recv(Fd, 0),

							% Lookup session
							gen_udp:send(Fd, ?RTPPROXY_IP, ?RTPPROXY_PORT, <<Cookie/binary, " Lc0,8,18,101 ", CallId/binary, " ", "192.0.43.11 54321 ", TagFrom/binary, ";1", " ", TagTo/binary, ";1", "\n">>),
							{ok, {?RTPPROXY_IP, ?RTPPROXY_PORT, _}} = gen_udp:recv(Fd, 0),

							% Select two Pids
							Ret0 = gproc:select([{ { {p,g, media} , '$1' , {CallId, '_', '_', '_', '_', '_'} }, [], ['$1']}]),

							% Wait enough for triggering timeout
							timer:sleep(3000),

							% This must return empty list
							Ret1 = gproc:select([{ { {p,g, media} , '$1' , {CallId, '_', '_', '_', '_', '_'} }, [], ['$1']}]),

							?assertMatch({[_, _], [true, true], []}, {Ret0, lists:map(fun(X) -> is_pid(X) end, Ret0), Ret1})
					end
				}
			]
		}
	}.
