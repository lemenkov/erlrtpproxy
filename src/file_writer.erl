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

-export([start_link/0]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).


start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
	Ets = ets:new(file_writer, [public, named_table]),
	error_logger:info_msg("File Writer: started at ~p.~n", [self()]),
	{ok, Ets}.

handle_call(Request, _From, State) ->
	error_logger:warning_msg("File Writer: strange call [~p]", [Request]),
	{reply, ok, State}.

handle_cast({{Type, Payload}, CallId, MediaId, Tag}, Ets) ->
	Fd = case ets:lookup(Ets, {CallId, MediaId, Tag}) of
		[] ->
			Filename = "/tmp/capture_cid_" ++ to_list(CallId) ++ "_mid_" ++ to_list(MediaId) ++ "_tag_" ++ to_list(Tag) ++ "." ++ to_list(Type),
			{ok, F} = file:open(Filename, [raw, write, append]),
			ets:insert_new(Ets, {{CallId, MediaId, Tag}, F}),
			F;
		[{{CallId, MediaId, Tag}, F}] -> F
	end,
	file:write(Fd, Payload),
	{noreply, Ets};

handle_cast({eof, CallId, MediaId, Tag}, Ets) ->
	case ets:lookup(Ets, {CallId, MediaId, Tag}) of
		[] -> ok;
		[Fd] -> file:close(Fd)
	end,
	{noreply, Ets};

handle_cast(Msg, State) ->
	error_logger:warning_msg("File Writer: strange cast: ~p.~n", [Msg]),
	{noreply, State}.

handle_info(Info, State) ->
	error_logger:warning_msg("File Writer: strange info: ~p.~n", [Info]),
	{noreply, State}.

terminate(_Reason, Ets) ->
	lists:foreach(fun([X]) -> file:close(X) end, ets:match(Ets, {{'_', '_', '_'},'$1'})),
	error_logger:info_msg("File Writer: stopped.~n"),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%
%% Private functions
%%

to_list(String) when is_list(String) -> String;
to_list(Binary) when is_binary(Binary) -> binary_to_list(Binary);
to_list(Integer) when is_integer(Integer) -> integer_to_list(Integer);
to_list(Atom) when is_atom(Atom) -> atom_to_list(Atom).
