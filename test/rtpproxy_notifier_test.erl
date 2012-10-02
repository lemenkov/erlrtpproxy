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

-module(rtpproxy_notifier_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/common.hrl").

-define(RTPPROXY_IP, {127,0,0,1}).
-define(RTPPROXY_PORT, 33333).

rtpproxy_notifier_backend_notify_test_() ->

	%%
	%% This is the socket which will be used for receiving notifications messages
	%%

	{ok, Fd} = gen_udp:open(0, [{active, false}, binary]),
	{ok, {_, Port}} = inet:sockname(Fd),

	% Common notification info
	CallId = <<"smaalefzrxxfrqw@localhost.localdomain-0">>,
	MediaId = <<"0">>,
	NotifyTag = <<"27124048">>,
	NotifyInfo = [{addr, {?RTPPROXY_IP, Port}}, {tag, NotifyTag}],

	{setup,
		fun() ->
				%%
				%% Set node name
				%%

				net_kernel:start(['rtpproxy_notifier_test@localhost', longnames]),

				%%
				%% Set necessary options
				%% (normally we'll set them in the /etc/erlrtpproxy.config
				%%

				application:set_env(rtpproxy, radacct_servers, [[{127,0,0,1},1813,"testradacctpass"]]),
				application:set_env(rtpproxy, notify_servers, udp),
				application:set_env(rtpproxy, ignore_start, true),
				application:set_env(rtpproxy, ignore_stop, true),

				%%
				%% Start real Notification backend
				%%

				rtpproxy_notifier_sup:start_link()
		end,
		fun (_) ->
				gen_udp:close(Fd),
				gen_server:cast({global, rtpproxy_notifier}, stop),
				net_kernel:stop()
		end,
		[
			{"Test start notification (should be filtered)",
				fun() ->
						gen_server:cast({global, rtpproxy_notifier}, {start, CallId, MediaId, NotifyInfo}),
						?assertEqual({error,timeout}, gen_udp:recv(Fd, 0, 1000))
				end
			},
			{"Test interim update notification",
				fun() ->
						gen_server:cast({global, rtpproxy_notifier}, {interim_update, CallId, MediaId, NotifyInfo}),
						{ok, {_, _, Answer}} = gen_udp:recv(Fd, 0, 1000),
						?assertEqual(NotifyTag, Answer)
				end
			},
			{"Test stop notification (should be filtered)",
				fun() ->
						gen_server:cast({global, rtpproxy_notifier}, {stop, CallId, MediaId, NotifyInfo}),
						?assertEqual({error,timeout}, gen_udp:recv(Fd, 0, 1000))
				end
			}
		]
	}.
