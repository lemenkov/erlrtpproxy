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

-module(test_utils).
-export([set_default_opts/0]).
-export([start/0]).
-export([stop/0]).

-define(RTPPROXY_IP, {127,0,0,1}).
-define(RTPPROXY_PORT, 33333).

start() ->
	%% Load main module
	application:start(rtpproxy).

%%
%% Set necessary options
%% (normally we'll set them in the /etc/erlrtpproxy.config
%%

set_default_opts() ->
	%% Set up backend's type (SER for now)
	application:set_env(rtpproxy, backend, ser),

	%% Options for SER backend
	application:set_env(rtpproxy, listen, {udp, "127.0.0.1", ?RTPPROXY_PORT}),

	%% Options for notification backend
	application:unset_env(rtpproxy, radacct_servers),
	application:unset_env(rtpproxy, notify_servers),
	application:set_env(rtpproxy, ignore_start, false),
	application:set_env(rtpproxy, ignore_stop, false),

	%% Options for rtpproxy itself
	application:set_env(rtpproxy, external, ?RTPPROXY_IP),
	application:set_env(rtpproxy, active, once),
	application:set_env(rtpproxy, sendrecv, roaming),
	application:unset_env(rtpproxy, internal),
	application:unset_env(rtpproxy, ipv6),
	application:set_env(rtpproxy, ttl, 30),
	application:set_env(rtpproxy, ttl_early, 30),
	application:set_env(rtpproxy, http_port, 8442),

	ok.

stop() ->
	application:stop(rtpproxy),
	net_kernel:stop().
