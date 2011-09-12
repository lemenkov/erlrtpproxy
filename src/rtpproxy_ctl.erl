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

-module(rtpproxy_ctl).
-author('lemenkov@gmail.com').

-export([start/0]).
-export([stop/0]).
-export([status/0]).

start() ->
	% Start our pool
	{ok,[[ConfigPath]]} = init:get_argument(config),
	Nodes = pool:start(rtpproxy, " -config " ++ ConfigPath ++ " "),

	application:start(erlsyslog),
	% Load erlsyslog parameters
	{ok, {SyslogHost, SyslogPort}} = application:get_env(erlsyslog, syslog_address),
	% Replace logger with erlsyslog
	error_logger:add_report_handler(erlsyslog, {0, SyslogHost, SyslogPort}),
	rpc:multicall(Nodes, error_logger, add_report_handler, [erlsyslog, {0, SyslogHost, SyslogPort}]),

	% Run Notifier on each node
	application:start(rtpproxy_notifier),
	rpc:multicall(Nodes, application, start, [rtpproxy_notifier]),

%	mnesia:create_schema([node()]),
%	mnesia:start(),
%	mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),

	% Load necessary config files
	rpc:multicall(Nodes, application, load, [rtpproxy]),

	% Load main module
	pool:pspawn(application, start, [rtpproxy]).


stop() ->
	Status = case init:get_plain_arguments() of
		[NodeStr] ->
			Node = list_to_atom(NodeStr),
			try rpc:call(Node, application, stop, [rtpproxy], 5000) of
				{badrpc, _} ->
					2;
				_ ->
					case rpc:call(Node, init, stop, [], 5000) of
						{badrpc, _} ->
							2;
						_ ->
							0
					end
			catch _:_ ->
				2
			end;
		_ ->
			1
	end,
	halt(Status).

status() ->
	Status = case init:get_plain_arguments() of
		[NodeStr] ->
			Node = list_to_atom(NodeStr),
			try rpc:call(Node, application, get_application, [rtpproxy], 5000) of
				{badrpc, _} ->
					4;
				{ok, rtpproxy} ->
					% No idea why but w/o this sleep call to gen_server:call/2 fails
					timer:sleep(2000),
					Ret = gen_server:call({global,rtpproxy}, status),
					ok = io:format("~s", [lists:flatten(Ret)]),
					0;
				undefined ->
					ok = io:format("~n"),
					3
			catch _:_ ->
				ok = io:format("~n"),
				4
			end;
		_ ->
			ok = io:format("~n"),
			4
	end,
	halt(Status).

