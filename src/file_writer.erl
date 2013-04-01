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

-module(file_writer).
-author('lemenkov@gmail.com').

-behaviour(gen_server).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include_lib("rtplib/include/rtp.hrl").

init([C, M, T]) ->
	process_flag(trap_exit, true),
	Filename = "/tmp/capture_cid_" ++ to_list(C) ++ "_mid_" ++ to_list(M) ++ "_tag_" ++ to_list(T),
	error_logger:info_msg("file writer: ~p - started at ~p.~n", [self(), node()]),
	{ok, {filename, Filename}}.

handle_call(Call, _From, State) ->
	error_logger:error_msg("File Writer: ~p - strange call [~p]", [self(), Call]),
	{stop, {error, {unknown_call, Call}}, State}.

handle_cast({Pkt, _, _}, State) when is_binary(Pkt) ->
	{ok, Rtp} = rtp:decode(Pkt),
	handle_cast({Rtp, null, null}, State);

handle_cast({#rtp{payload_type = Type}, _, _} = Cast, {filename, Filename}) ->
	{ok, Fd} = file:open(Filename ++ "." ++ to_list(Type), [raw, write, append]),
	handle_cast(Cast, Fd);

handle_cast({#rtp{payload = Payload}, _, _}, Fd) ->
	file:write(Fd, Payload),
	{noreply, Fd};

handle_cast(Cast, State) ->
	error_logger:error_msg("file writer: ~p - strange cast: ~p.~n", [self(), Cast]),
	{stop, {error, {unknown_cast, Cast}}, State}.

handle_info(Info, State) ->
	error_logger:error_msg("file writer: ~p - strange info: ~p.~n", [self(), Info]),
	{stop, {error, {unknown_info, Info}}, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, {filename, _}) ->
	{memory, Bytes} = erlang:process_info(self(), memory),
	error_logger:info_msg("file writer: ~p - terminated due to reason [~p] (allocated ~b bytes)", [self(), Reason, Bytes]);
terminate(Reason, Fd) ->
	{memory, Bytes} = erlang:process_info(self(), memory),
	file:close(Fd),
	error_logger:info_msg("file writer: ~p - terminated due to reason [~p] (allocated ~b bytes)", [self(), Reason, Bytes]).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions %%
%%%%%%%%%%%%%%%%%%%%%%%%

to_list(String) when is_list(String) -> String;
to_list(Binary) when is_binary(Binary) -> binary_to_list(Binary);
to_list(Integer) when is_integer(Integer) -> integer_to_list(Integer);
to_list(Atom) when is_atom(Atom) -> atom_to_list(Atom).
