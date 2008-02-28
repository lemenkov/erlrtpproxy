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

-module(call).
-author('lemenkov@gmail.com').

-behaviour(gen_server).
-export([start/1]).
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-record(party, {fdfrom=null, origipfrom=null, origportfrom=null, tagfrom=null, mediaidfrom=null,
		fdto=null,   origipto=null,   origportto=null,   tagto=null,   mediaidto=null}).

start(Args) ->
	gen_server:start(call, Args, []).

start_link(Args) ->
	gen_server:start_link(call, Args, []).

init (_Unused) ->
	process_flag(trap_exit, true),
	{ok, Name} = inet:gethostname(),
	{ok, MainIp} = inet:getaddr(Name, inet),
	WatcherPid = spawn (rtpsocket, watcher, [self()]),
	io:format ("::: call[~w] thread started.~n", [self()]),
	{ok, {MainIp, WatcherPid, []}}.

% handle originate call leg (new media id possibly)
handle_call({message_u, {FromTag, MediaId}}, _From, {MainIp, WatcherPid, Parties}) ->
	io:format ("::: call[~w] message [U] MediaId [~s].~n", [self(), MediaId]),
	% search for already  existed
	case lists:keysearch(FromTag, #party.tagfrom, Parties) of
		% call already exists
		{value, Party} ->
			io:format("::: call[~w] Already exists!~n", [self()]),
			{ok, {LocalIp, LocalPort}} = inet:sockname(Party#party.fdfrom),
			Reply = " " ++ integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
			io:format("::: call[~w] answer [~s]~n", [self(), Reply]),
			{reply, {ok, Reply}, {MainIp, WatcherPid, Parties}};
		false ->
			% open new Fd and attach it
			case gen_udp:open(0, [binary, {ip, MainIp}, {active, true}]) of
				{ok, Fd} ->
					io:format("::: call[~w] Create new socket... OK~n", [self()]),
					gen_udp:controlling_process(Fd, WatcherPid),

					{ok, {LocalIp, LocalPort}} = inet:sockname(Fd),
					NewParty = #party{fdfrom=Fd, tagfrom=FromTag, mediaidfrom=MediaId},

					Reply = " " ++ integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
					io:format("::: call[~w] answer [~s]~n", [self(), Reply]),
					{reply, {ok, Reply}, {MainIp, WatcherPid, lists:append(Parties, [NewParty])}};
				{error, Reason} ->
					io:format("::: call[~w] Create new socket FAILED [~p]~n", [self(), Reason]),
					{reply, {error, udp_error}, {MainIp, WatcherPid, Parties}}
			end
	end;

% handle answered call leg
handle_call({message_l, {FromTag, MediaIdFrom, ToTag, MediaIdTo}}, _From, {MainIp, WatcherPid, Parties}) ->
	io:format ("::: call[~w] message [L] MediaIdFrom [~s] MediaIdTo[~s].~n", [self(), MediaIdFrom, MediaIdTo]),
	% search for already  existed
	case lists:keysearch(FromTag, #party.tagfrom, Parties) of
		% call already exists and Fd is not opened
		{value, Party} when Party#party.fdto == null ->
			io:format("::: call[~w] Already exists!~n", [self()]),
			case gen_udp:open(0, [binary, {ip, MainIp}, {active, true}]) of
				{ok, Fd} ->
					io:format(" OK~n"),
					gen_udp:controlling_process(Fd, WatcherPid),

					{ok, {LocalIp, LocalPort}} = inet:sockname(Fd),
					NewParty = Party#party{fdto=Fd, tagto=ToTag, mediaidto=MediaIdTo},

					Reply = " " ++ integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
					io:format("::: call[~w] answer [~s]~n", [self(), Reply]),
					{reply, {ok, Reply}, {MainIp, WatcherPid, lists:keyreplace(FromTag, #party.tagfrom, Parties, NewParty)}};
				{error, Reason} ->
					io:format(" FAILED [~p]~n", [Reason]),
					{reply, {error, udp_error}, {MainIp, WatcherPid, Parties}}
			end;
		% call already exists and Fd is opened
		{value, Party} when Party#party.fdto /= null ->
			io:format("::: call[~w] Already exists!~n", [self()]),

			{ok, {LocalIp, LocalPort}} = inet:sockname(Party#party.fdto),

			Reply = " " ++ integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
			io:format("::: call[~w] answer [~s]~n", [self(), Reply]),
			{reply, {ok, Reply}, {MainIp, WatcherPid, Parties}};
		false ->
			% Call not found.
			io:format("::: call[~w] ERROR not found~n", [self()]),
			{reply, {error, not_found}, {MainIp, WatcherPid, Parties}}
	end;

handle_call(_Other, _From, State) ->
	{noreply, State}.

% rtp from some port
handle_cast({udp, {Fd, Ip, Port, Msg}}, {MainIp, WatcherPid, Parties}) ->
	case lists:keysearch(Fd, #party.fdfrom, Parties) of
		{value, Party} ->
			if
				Party#party.origipto /= null, Party#party.origportto /= null ->
%					io:format("::: cast[~w] rtp from FdFrom and we can send~n", [self()]),
					gen_udp:send(Party#party.fdto, Party#party.origipto, Party#party.origportto, Msg);
				true ->
%					io:format("::: cast[~w] rtp from FdFrom and we CANNOT send yet~n", [self()]),
					ok
			end,
			if
				Party#party.origipfrom == null, Party#party.origportfrom == null ->
					NewParty = Party#party{origipfrom=Ip, origportfrom=Port},
					{noreply, {MainIp, WatcherPid, lists:keyreplace(Fd, #party.fdfrom, Parties, NewParty)}};
				true ->
					{noreply, {MainIp, WatcherPid, Parties}}
			end;
		false ->
			case lists:keysearch(Fd, #party.fdto, Parties) of
				{value, Party} ->
					if
						Party#party.origipfrom /= null, Party#party.origportfrom /= null ->
%							io:format("::: cast[~w] rtp to FdTo and we can send~n", [self()]),
							gen_udp:send(Party#party.fdfrom, Party#party.origipfrom, Party#party.origportfrom, Msg);
						true ->
%							io:format("::: cast[~w] rtp to FdTo and we CANNOT send yet~n", [self()]),
							ok
					end,
					if
						Party#party.origipto == null, Party#party.origportto == null ->
							NewParty = Party#party{origipto=Ip, origportto=Port},
							{noreply, {MainIp, WatcherPid, lists:keyreplace(Fd, #party.fdto, Parties, NewParty)}};
						true ->
							{noreply, {MainIp, WatcherPid, Parties}}
					end;
				false ->
					{noreply, {MainIp, WatcherPid, Parties}}
			end
	end;

handle_cast({WatcherPid, timeout}, {MainIp, WatcherPid, Parties}) ->
	{stop, timeout, {MainIp, WatcherPid, Parties}};

handle_cast(message_d, State) ->
	{stop, message_d, State};

handle_cast(message_r, State) ->
	% TODO start recording of RTP
	{noreply, State};

handle_cast(message_s, State) ->
	% TODO stop recording of RTP
	{noreply, State};

% all other messages
handle_cast(_Request, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, {_MainIp, WatcherPid, Parties}) ->
	ok = gen_server:cast({global, rtpproxy}, {call_terminated, {self(), Reason}}),
	WatcherPid ! {self(), destroy},
	lists:foreach(
			fun(X)  ->
				if
					X#party.fdfrom /= null ->
						gen_udp:close(X#party.fdfrom);
					true -> ok
				end,
				if
					X#party.fdto /= null ->
						gen_udp:close(X#party.fdto);
					true -> ok
				end
			end,
			Parties),
	io:format("::: call[~w] thread terminated due to reason [~w]~n", [self(), Reason]).

handle_info(Info, State) ->
	io:format("::: call[~w] Info [~w]~n", [self(), Info]),
	{noreply, State}.

