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

-module(storage).
-author('lemenkov@gmail.com').

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include_lib("kernel/include/file.hrl"). % for #file_descriptor{}

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
	Ets = ets:new(mmap, [public, named_table]),
        error_logger:info_msg("Storage: started at ~p.~n", [self()]),
	{ok, Ets}.

handle_call({get, Filename}, _From, Ets) ->
	FI = case ets:lookup(Ets, Filename) of
		[] ->
			{ok, FileInfo} = file:read_file_info(Filename),
			{ok, Fd} = emmap:open(Filename, [read, shared, direct, nolock]),
			ets:insert_new(Ets, {Filename, {Fd, FileInfo#file_info.size}}),
			{Fd, FileInfo#file_info.size};
		[{Filename, F}] ->
			F
	end,
        {reply, {ok, FI}, Ets};

handle_call(Request, _From, State) ->
	error_logger:warning_msg("Storage: strange call [~p]", [Request]),
        {reply, ok, State}.

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(Msg, State) ->
        error_logger:warning_msg("Storage: strange cast: ~p.~n", [Msg]),
        {noreply, State}.

handle_info(Info, State) ->
        error_logger:warning_msg("Storage: strange info: ~p.~n", [Info]),
        {noreply, State}.

terminate(_Reason, Ets) ->
	lists:foreach(fun([X]) -> file:close(X) end, ets:match(Ets, {'_', {'$1', '_'}})),
        error_logger:info_msg("Storage: stopped.~n"),
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.
