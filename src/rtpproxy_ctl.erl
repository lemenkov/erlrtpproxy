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

-export([upgrade/0]).
-export([upgrade/1]).
-export([stop/0]).
-export([status/0]).

upgrade() ->
	Status = case init:get_plain_arguments() of
		[NodeStr] ->
			Node = list_to_atom(NodeStr),
			try rpc:call(Node, gen_server, cast, [{global, rtpproxy}, upgrade], 5000) of
				{badrpc, Reason} ->
					2;
				_ ->
					0
			catch _:_ ->
				2
			end;
		_ ->
			1
	end,
	halt(Status).

upgrade(Nodes) when is_list(Nodes) ->
	lists:foreach(fun upgrade/1, Nodes);

upgrade(Node) when is_atom(Node) ->
	{ok, Sources} = application:get_env(rtpproxy, sources),
	lists:foreach(fun(Mod) ->
			{Mod, Bin, File} = code:get_object_code(Mod),
			rpc:call(Node, code, load_binary, [Mod, File, Bin])
		end,
	Sources).

stop() ->
	Status = case init:get_plain_arguments() of
		[NodeStr] ->
			Node = list_to_atom(NodeStr),
			try rpc:call(Node, application, stop, [rtpproxy], 5000) of
				{badrpc, Reason} ->
					2;
				_ ->
					case rpc:call(Node, init, stop, [], 5000) of
						{badrpc, Reason} ->
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
				{badrpc, Reason} ->
					4;
				{ok, rtpproxy} ->
					rpc:call(Node, gen_server, cast, [{global,rtpproxy}, status], 5000),
					0;
				undefined ->
					3
			catch _:_ ->
				4
			end;
		_ ->
			4
	end,
	halt(Status).

