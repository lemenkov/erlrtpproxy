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

-record(party, {fdfrom=null, ipfrom=null, portfrom=null, tagfrom=null,
		ipfromguess=null, portfromguess=null,
		fdto=null,   ipto=null,   portto=null,   tagto=null,
		mediaid=null, pid=null, answered=false}).

start(MainIpAtom) when is_atom(MainIpAtom) ->
	gen_server:start(?MODULE, MainIpAtom, []).

start_link(MainIpAtom) when is_atom(MainIpAtom) ->
	gen_server:start_link(?MODULE, MainIpAtom, []).

init (MainIpAtom) ->
	process_flag(trap_exit, true),
	{ok, MainIp} = inet_parse:address(atom_to_list(MainIpAtom)),
	io:format ("::: call[~w] thread started at ~s~n", [self(), inet_parse:ntoa(MainIp)]),
	{ok, {MainIp, []}}.

% handle originate call leg (new media id possibly)
handle_call({message_u, {OrigIp, OrigPort, FromTag, MediaId}}, _From, {MainIp, Parties}) ->
	io:format ("::: call[~w] message [U] MediaId [~s].~n", [self(), MediaId]),
	% search for already  existed
	case lists:keysearch(MediaId, #party.mediaid, Parties) of
		% call already exists
		{value, Party} ->
			io:format("::: call[~w] Already exists!~n", [self()]),
			{ok, {LocalIp, LocalPort}} = inet:sockname(Party#party.fdfrom),
			Reply = " " ++ integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
			io:format("::: call[~w] answer [~s]~n", [self(), Reply]),
			{reply, {ok, Reply}, {MainIp, Parties}};
		false ->
			% open new FdFrom and attach it
			case gen_udp:open(0, [binary, {ip, MainIp}, {active, true}]) of
				{ok, Fd1} ->
					io:format("::: call[~w] Create new socket... OK~n", [self()]),

					% ty to create FdTo also
					Fd2 = case gen_udp:open(0, [binary, {ip, MainIp}, {active, true}]) of
						{ok, Fd} ->
							% Create another socket also
							Fd;
						_ ->
							% Create another socket failed
							null
					end,
					{ok, {LocalIp, LocalPort}} = inet:sockname(Fd1),

					{ok, GuessIp} = inet_parse:address(OrigIp),
					{ok, [GuessPort], _} = io_lib:fread("~u", OrigPort),
%					io:format("::: call[~w] Guessing caller listens at ~w:~w~n", [self(), GuessIp, GuessPort]),

					NewParty = #party{fdfrom=Fd1, ipfromguess=GuessIp, portfromguess=GuessPort, tagfrom=FromTag, fdto=Fd2, mediaid=MediaId},

					Reply = " " ++ integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
					io:format("::: call[~w] answer [~s]~n", [self(), Reply]),
					{reply, {ok, Reply}, {MainIp, lists:append(Parties, [NewParty])}};
				{error, Reason} ->
					io:format("::: call[~w] Create new socket FAILED [~p]~n", [self(), Reason]),
					{reply, {error, udp_error}, {MainIp, Parties}}
			end
	end;

% handle answered call leg
% Both MediaId's are equal (just guessing)
handle_call({message_l, {FromTag, MediaId, ToTag, MediaId}}, _From, {MainIp, Parties}) ->
	io:format ("::: call[~w] message [L] MediaId [~s].~n", [self(), MediaId]),
	% search for already  existed
	case lists:keysearch(MediaId, #party.mediaid, Parties) of
		% call already exists and Fd is not opened
		{value, Party} when Party#party.fdto == null ->
			io:format("::: call[~w] Already exists (FdTo == NULL)!~n", [self()]),
			case gen_udp:open(0, [binary, {ip, MainIp}, {active, true}]) of
				{ok, Fd} ->
					{ok, {LocalIp, LocalPort}} = inet:sockname(Fd),
					NewParty = Party#party{fdto=Fd, tagto=ToTag, answered=true},

					Reply = " " ++ integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
%					io:format("::: call[~w] answer [~s]~n", [self(), Reply]),
					{reply, {ok, Reply}, {MainIp, lists:keyreplace(MediaId, #party.mediaid, Parties, NewParty)}};
				{error, Reason} ->
					io:format(" FAILED [~p]~n", [Reason]),
					{reply, {error, udp_error}, {MainIp, Parties}}
			end;
		% call already exists and Fd is opened
		{value, Party} when Party#party.fdto /= null ->
			io:format("::: call[~w] Already exists (FdTo != NULL)!~n", [self()]),

			{ok, {LocalIp, LocalPort}} = inet:sockname(Party#party.fdto),
			NewParty = Party#party{answered=true},

			Reply = " " ++ integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
%			io:format("::: call[~w] answer [~s]~n", [self(), Reply]),
			{reply, {ok, Reply}, {MainIp, lists:keyreplace(MediaId, #party.mediaid, Parties, NewParty)}};
		false ->
			% Call not found.
			io:format("::: call[~w] ERROR not found~n", [self()]),
			{reply, {error, not_found}, {MainIp, Parties}}
	end;
%handle_call(message_i, _From, State) ->
%	{reply, {ok, Reply}, State};

handle_call(_Other, _From, State) ->
	{noreply, State}.

handle_cast(message_d, State) ->
	{stop, message_d, State};

handle_cast(message_r, State) ->
	% TODO start recording of RTP
	{noreply, State};

handle_cast(message_s, State) ->
	% TODO stop recording of RTP
	{noreply, State};

handle_cast({stop, Pid}, {MainIp, Parties}) ->
%	io:format("::: call[~w] TIMEOUT when state is [~p]~n", [self(), Parties]),
	case lists:keytake (Pid, #party.pid, Parties) of
		{value, Party, []} ->
%			io:format("::: call[~w] It was the last mediastream - exiting~n", [self()]),
			{stop, stop, {MainIp, []}};
		{value, Party, NewParties} ->
%			io:format("::: call[~w] It was NOT the last mediastream~n", [self()]),
			{noreply, {MainIp, NewParties}};
		false ->
			io:format("::: call[~w] Cannot find such Pid~n", [self()]),
			{noreply, {MainIp, Parties}}
	end;

% all other messages
handle_cast(_Request, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, {_MainIp, Parties}) ->
	gen_server:cast({global, rtpproxy}, {call_terminated, {self(), Reason}}),
	% TODO clean parties
	_Unused = lists:foreach(
		fun(X)  ->
			if
				X#party.pid /= null ->
					gen_server:cast(X#party.pid, stop);
				true ->
					ok
			end
		end,
		Parties),
	io:format("::: call[~w] thread terminated due to reason [~p]~n", [self(), Reason]).

% rtp from some port
handle_info({udp, Fd, Ip, Port, Msg}, {MainIp, Parties}) ->
%	io:format("::: call[~w] udp from Fd [~w] [~p:~p]~n", [self(), Fd, Ip, Port]),
	Fun1 = fun (X,Y1,Y2,Z) ->
		case lists:keysearch(X,Y1,Z) of
			{value, Val1} ->
				{value, Val1};
			false ->
				case lists:keysearch (X,Y2,Z) of
					{value, Val2} ->
						{value, Val2};
					false ->
						false
				end
		end
	end,
	case Fun1(Fd, #party.fdfrom, #party.fdto, Parties) of
		{value, Party} when Party#party.fdto == Fd, Party#party.ipto /= null, Party#party.portto /= null, Party#party.pid == null, Party#party.answered == true ->
			% We don't need to check FdFrom for existence since it's no doubt exists
%			io:format("::: cast[~w] rtp to FdTo [~w] from [~w:~w] and we can send~n", [self(), Fd, Ip, Port]),
			{ok, PartyPid} = media:start({self(), {Party#party.fdfrom, Ip, Port}, {Party#party.fdto, Party#party.ipto, Party#party.portto}}),
			% FIXME send this Msg
			gen_udp:controlling_process(Party#party.fdfrom, PartyPid),
			gen_udp:controlling_process(Party#party.fdto, PartyPid),
			NewParty = Party#party{ipfrom=Ip, portfrom=Port, pid=PartyPid},
			{noreply, {MainIp, lists:keyreplace(Fd, #party.fdto, Parties, NewParty)}};
		{value, Party} when Party#party.fdto == Fd ->
%			io:format("::: cast[~w] rtp to FdTo [~w] from [~w:~w] and we CANNOT send yet~n", [self(), Fd, Ip, Port]),
			NewParty = Party#party{ipfrom=Ip, portfrom=Port},
			{noreply, {MainIp, lists:keyreplace(Fd, #party.fdto, Parties, NewParty)}};
		{value, Party} when Party#party.fdfrom == Fd, Party#party.ipfrom /= null, Party#party.portfrom /= null, Party#party.fdto /= null, Party#party.pid == null, Party#party.answered == true ->
%			io:format("::: cast[~w] rtp to FdFrom [~w] from [~w:~w] and we can send~n", [self(), Fd, Ip, Port]),
			{ok, PartyPid} = media:start({self(), {Party#party.fdfrom, Party#party.ipfrom, Party#party.portfrom}, {Party#party.fdto, Ip, Port}}),
			% FIXME send this Msg
			gen_udp:controlling_process(Party#party.fdfrom, PartyPid),
			gen_udp:controlling_process(Party#party.fdto, PartyPid),
			NewParty = Party#party{ipto=Ip, portto=Port, pid=PartyPid},
			{noreply, {MainIp, lists:keyreplace(Fd, #party.fdfrom, Parties, NewParty)}};
		{value, Party} when Party#party.fdfrom == Fd ->
%			io:format("::: cast[~w] rtp to FdFrom [~w] from [~w:~w] and we CANNOT send yet~n", [self(), Fd, Ip, Port]),
			% guessing that caller has uPnP
			case Party#party.fdto of
				null ->
					ok;
				_ ->
					gen_udp:send(Party#party.fdto, Party#party.ipfromguess, Party#party.portfromguess, Msg)
			end,
			NewParty = Party#party{ipto=Ip, portto=Port},
			{noreply, {MainIp, lists:keyreplace(Fd, #party.fdfrom, Parties, NewParty)}};
		false ->
			{noreply, {MainIp, Parties}}
	end;

handle_info(Info, State) ->
	io:format("::: call[~w] Info [~w]~n", [self(), Info]),
	{noreply, State}.

