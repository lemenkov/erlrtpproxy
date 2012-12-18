#!/usr/bin/escript
%% -*- erlang -*-
%%! -pa ebin +K true +A 4 -smp enable

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

%start() ->
%	{ok,[[ConfigPath]]} = init:get_argument(config),

	%% Start our pool
%	Nodes = [node()|pool:start(rtpproxy, " -config " ++ ConfigPath ++ " ")],

	%% Replace logger with erlsyslog
%	rpc:multicall(Nodes, error_logger, add_report_handler, [erlsyslog]),

	%% Run gproc on each node
%	rpc:multicall(Nodes, application, set_env, [gproc, gproc_dist, all]),
%	rpc:multicall(Nodes, application, start, [gproc]),

	%% Load necessary config files
%	rpc:multicall(Nodes, application, load, [rtpproxy]),

	%% Load main module
%	application:start(rtpproxy).

%status() ->
%	Node = case init:get_plain_arguments() of
%		[NodeStr] ->
%			list_to_atom(NodeStr);
%		_ ->
%			halt(1)
%	end,
%	case call(Node, rtpproxy_ctl, command, [#cmd{type = ?CMD_I, params = [brief]}], 4) of
%		{ok, {stats, Number}} ->
%			io:format("active calls: ~p~n", [Number]),
%			halt(0);
%		undefined ->
%			ok = io:format("~n"),
%			halt(3)
%	end.

call_remote(Node, M, F, A, HaltRet) ->
	try rpc:call(Node, M, F, A, 5000) of
		{badrpc, _} ->
			halt(HaltRet);
		Rest ->
			Rest
	catch _:_ ->
		halt(HaltRet)
	end.

main(["stop", "-node", NodeStr]) ->
	Node = list_to_atom(NodeStr),
	net_kernel:start(['erlrtpproxy-stop@127.0.0.1',longnames]),
	pong = net_adm:ping(Node),
	call_remote(Node, rtpproxy_ctl, stop, [], 2),
	timer:sleep(1000),
	halt(0);

main(["status", "-node", NodeStr]) ->
	Node = list_to_atom(NodeStr),
	net_kernel:start(['erlrtpproxy-status@127.0.0.1',longnames]),
	case net_adm:ping(Node) of
		pong -> halt(0);
		pang -> halt(1)
	end;

main(Arg) ->
	error_logger:info_msg("ARG: ~p~n", [Arg]),
	halt(0).
