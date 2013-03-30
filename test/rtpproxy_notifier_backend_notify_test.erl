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

-module(rtpproxy_notifier_backend_notify_test).

-include_lib("eunit/include/eunit.hrl").

-include("../src/common.hrl").

-define(RTPPROXY_IP, {127,0,0,1}).
-define(RTPPROXY_PORT, 33333).

rtpproxy_notifier_backend_notify_udp_test_DISABLE() ->

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

				test_utils:set_default_opts(),
				application:set_env(rtpproxy, notify_servers, udp),
				application:set_env(rtpproxy, ignore_start, true),
				application:set_env(rtpproxy, ignore_stop, true),

				%%
				%% Start rtpproxy
				%%

				test_utils:start()
		end,
		fun (_) ->
				gen_udp:close(Fd),

				%%
				%% Stop rtpproxy
				%%

				test_utils:stop()
		end,
		[
			{"Test start notification (should be filtered)",
				fun() ->
						rtpproxy_ctl:acc(start, CallId, MediaId, NotifyInfo),
						?assertEqual({error,timeout}, gen_udp:recv(Fd, 0, 1000))
				end
			},
			{"Test interim update notification",
				fun() ->
						rtpproxy_ctl:acc(interim_update, CallId, MediaId, NotifyInfo),
						{ok, {_, _, Answer}} = gen_udp:recv(Fd, 0, 1000),
						?assertEqual(NotifyTag, Answer)
				end
			},
			{"Test stop notification (should be filtered)",
				fun() ->
						rtpproxy_ctl:acc(stop, CallId, MediaId, NotifyInfo),
						?assertEqual({error,timeout}, gen_udp:recv(Fd, 0, 1000))
				end
			}
		]
	}.

rtpproxy_notifier_backend_notify_tcp_test_DISABLE() ->

	%%
	%% This is the socket which will be used for receiving notifications messages
	%%

	{ok, Fd} = gen_tcp:listen(0, [{active, false}, binary]),
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

				test_utils:set_default_opts(),
				application:set_env(rtpproxy, notify_servers, tcp),
				application:set_env(rtpproxy, ignore_start, true),

				%%
				%% Start socket accepting thread
				%%

				spawn(fun() ->
							ets:new(rtpproxy_notifier_backend_notify_tcp_test_ets, [public, named_table]),
							accept(rtpproxy_notifier_backend_notify_tcp_test_ets, Fd)
					end),

				%%
				%% Start rtpproxy
				%%

				test_utils:start()
		end,
		fun (_) ->
				gen_tcp:close(Fd),

				%%
				%% Stop rtpproxy
				%%

				test_utils:stop()
		end,
		[
			{"Test start notification (should be filtered)",
				fun() ->
						rtpproxy_ctl:acc(start, CallId, MediaId, NotifyInfo),
						% Poor man synchronization object
						timer:sleep(100),
						% No attempts to connect
						?assertEqual([],  ets:lookup(rtpproxy_notifier_backend_notify_tcp_test_ets, fd))
				end
			},
			{"Test stop notification",
				fun() ->
						rtpproxy_ctl:acc(stop, CallId, MediaId, NotifyInfo),
						% Poor man synchronization object
						timer:sleep(100),
						% We have a connection - let's tr to read from it
						[{fd,ActiveFd}] = ets:lookup(rtpproxy_notifier_backend_notify_tcp_test_ets, fd),
						{ok, Answer} = gen_tcp:recv(ActiveFd, 0, 1000),
						?assertEqual(NotifyTag, Answer)
				end
			}
		]
	}.

rtpproxy_notifier_backend_notify_tcp_two_addresses_test_DISABLE() ->

	%%
	%% This is the socket which will be used for receiving notifications messages
	%%

	%% First subscriber
	{ok, Fd1} = gen_tcp:listen(0, [{active, false}, binary]),
	{ok, {_, Port1}} = inet:sockname(Fd1),

	%% Second subscriber
	{ok, Fd2} = gen_tcp:listen(0, [{active, false}, binary]),
	{ok, {_, Port2}} = inet:sockname(Fd2),

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

				test_utils:set_default_opts(),
				application:set_env(rtpproxy, notify_servers, tcp),
				application:set_env(rtpproxy, ignore_start, true),

				%%
				%% Start two simultaneous socket accepting thread
				%%

				spawn(fun() ->
							ets:new(rtpproxy_notifier_backend_notify_tcp_test_1_ets, [public, named_table]),
							accept(rtpproxy_notifier_backend_notify_tcp_test_1_ets, Fd1)
					end),

				spawn(fun() ->
							ets:new(rtpproxy_notifier_backend_notify_tcp_test_2_ets, [public, named_table]),
							accept(rtpproxy_notifier_backend_notify_tcp_test_2_ets, Fd2)
					end),

				%%
				%% Start rtpproxy
				%%

				test_utils:start()
		end,
		fun (_) ->
				gen_tcp:close(Fd1),
				gen_tcp:close(Fd2),

				%%
				%% Stop rtpproxy
				%%

				test_utils:stop()
		end,
		[
			{"Test stop notification to the first subscriber",
				fun() ->
						% Common notification info
						CallId = <<"smaalefzrxxfrqw@localhost.localdomain-0__1">>,
						MediaId = <<"0">>,
						NotifyTag = <<"27124048_1">>,
						NotifyInfo = [{addr, {?RTPPROXY_IP, Port1}}, {tag, NotifyTag}],

						rtpproxy_ctl:acc(stop, CallId, MediaId, NotifyInfo),
						% Poor man synchronization object
						timer:sleep(100),
						% We have a connection - let's tr to read from it
						[{fd,ActiveFd}] = ets:lookup(rtpproxy_notifier_backend_notify_tcp_test_1_ets, fd),
						{ok, Answer} = gen_tcp:recv(ActiveFd, 0, 1000),
						?assertEqual(NotifyTag, Answer)
				end
			},
			{"Test stop notification to the second subscriber",
				fun() ->
						% Common notification info
						CallId = <<"smaalefzrxxfrqw@localhost.localdomain-0__2">>,
						MediaId = <<"0">>,
						NotifyTag = <<"27124048_2">>,
						NotifyInfo = [{addr, {?RTPPROXY_IP, Port2}}, {tag, NotifyTag}],

						rtpproxy_ctl:acc(stop, CallId, MediaId, NotifyInfo),
						% Poor man synchronization object
						timer:sleep(100),
						% We have a connection - let's tr to read from it
						[{fd,ActiveFd}] = ets:lookup(rtpproxy_notifier_backend_notify_tcp_test_2_ets, fd),
						{ok, Answer} = gen_tcp:recv(ActiveFd, 0, 1000),
						?assertEqual(NotifyTag, Answer)
				end
			}
		]
	}.

accept(Ets, Fd) ->
	case gen_tcp:accept(Fd) of
		{ok, Socket} ->
			ets:insert_new(Ets, {fd, Socket}),
			accept(Ets, Fd);
		_ ->
			case ets:lookup(Ets, fd) of
				[{fd,ActiveFd}] ->
					gen_tcp:close(ActiveFd);
				_ ->
					ok
			end,
			ets:delete(Ets)
	end.
