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

-include("common.hrl").

-record(party, {fdfrom=null, ipfrom=null, portfrom=null, tagfrom=null,
		ipfromguess=null, portfromguess=null,
		fdto=null,   ipto=null,   portto=null,   tagto=null,
		mediaid=null,
		pid=null,
		answered=false,
		mod_asymmetric=false,
		mod_e=false,
		mod_i=false,
		mod_ipv6=false,
		mod_symmetric=true,
		mod_weak=false,
		mod_z=false}).

start(MainIp) ->
	gen_server:start(?MODULE, MainIp, []).

start_link(MainIp) ->
	gen_server:start_link(?MODULE, MainIp, []).

init (MainIp) ->
	process_flag(trap_exit, true),
	?PRINT("started at ~s", [inet_parse:ntoa(MainIp)]),
	{ok, {MainIp, []}}.

% handle originate call leg (new media id possibly)
% TODO handle Modifiers
handle_call({message_u, {{GuessIp, GuessPort}, {FromTag, MediaId}, Modifiers}}, _From, {MainIp, Parties}) ->
	?PRINT ("message [U] MediaId [~b]", [MediaId]),
	% search for already  existed
	case lists:keysearch(MediaId, #party.mediaid, Parties) of
		% call already exists
		{value, Party} ->
			?PRINT("Already exists!", []),
			{ok, {LocalIp, LocalPort}} = inet:sockname(Party#party.fdfrom),
			Reply = integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
			?PRINT("answer [~s]", [Reply]),
			{reply, {ok, Reply}, {MainIp, Parties}};
		false ->
			% open new FdFrom and attach it
			case gen_udp:open(0, [binary, {ip, MainIp}, {active, true}]) of
				{ok, Fd1} ->
					?PRINT("Create new socket... OK", []),

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

%					?PRINT("Guessing caller listens at ~w:~w", [GuessIp, GuessPort]),

					NewParty = #party{fdfrom=Fd1, ipfromguess=GuessIp, portfromguess=GuessPort, tagfrom=FromTag, fdto=Fd2, mediaid=MediaId},

					Reply = integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
					?PRINT("answer [~s]", [Reply]),
					{reply, {ok, Reply}, {MainIp, lists:append(Parties, [NewParty])}};
				{error, Reason} ->
					?PRINT("Create new socket FAILED [~p]", [Reason]),
					{reply, {error, udp_error}, {MainIp, Parties}}
			end
	end;

% handle answered call leg
% Both MediaId's are equal (just guessing)
handle_call({message_l, {{FromTag, MediaId}, {ToTag, MediaId}, Modifiers}}, _From, {MainIp, Parties}) ->
	?PRINT ("message [L] MediaId [~b]", [MediaId]),
	% search for already  existed
	case lists:keysearch(MediaId, #party.mediaid, Parties) of
		% call already exists and Fd is not opened
		{value, Party} when Party#party.fdto == null ->
			?PRINT("Already exists (FdTo == NULL)!", []),
			case gen_udp:open(0, [binary, {ip, MainIp}, {active, true}]) of
				{ok, Fd} ->
					{ok, {LocalIp, LocalPort}} = inet:sockname(Fd),
					NewParty = Party#party{fdto=Fd, tagto=ToTag, answered=true},

					Reply = integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
%					?PRINT("answer [~s]", [Reply]),
					{reply, {ok, Reply}, {MainIp, lists:keyreplace(MediaId, #party.mediaid, Parties, NewParty)}};
				{error, Reason} ->
					?PRINT("FAILED [~p]", [Reason]),
					{reply, {error, udp_error}, {MainIp, Parties}}
			end;
		% call already exists and Fd is opened
		{value, Party} when Party#party.fdto /= null ->
			?PRINT("Already exists (FdTo != NULL)!", []),

			{ok, {LocalIp, LocalPort}} = inet:sockname(Party#party.fdto),
			NewParty = Party#party{answered=true},

			Reply = integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
%			?PRINT("answer [~s]", [Reply]),
			{reply, {ok, Reply}, {MainIp, lists:keyreplace(MediaId, #party.mediaid, Parties, NewParty)}};
		false ->
			% Call not found.
			?PRINT("ERROR not found", []),
			{reply, {error, not_found}, {MainIp, Parties}}
	end;

handle_call(message_i, _From, State) ->
	% TODO (acquire information about call state)
	{reply, {ok, "TODO"}, State};

handle_call(_Other, _From, State) ->
	{noreply, State}.

handle_cast(message_d, State) ->
	{stop, message_d, State};

handle_cast({message_r, filename}, State) ->
	% TODO start recording of RTP
	{noreply, State};

handle_cast({message_p, filename}, State) ->
	% TODO start playback of pre-recorded audio
	{noreply, State};

handle_cast(message_s, State) ->
	% TODO stop playback/recording of RTP
	{noreply, State};

% timeout from media stream
% TODO consider to stop all other media streams
handle_cast({stop, Pid}, {MainIp, Parties}) ->
%	?PRINT("TIMEOUT when state is [~p]", [Parties]),
	case lists:keytake (Pid, #party.pid, Parties) of
		{value, Party, []} ->
%			?PRINT("It was the last mediastream - exiting", []),
			{stop, stop, {MainIp, []}};
		{value, Party, NewParties} ->
%			?PRINT("It was NOT the last mediastream", []),
			{noreply, {MainIp, NewParties}};
		false ->
			?PRINT("Cannot find such Pid", []),
			{noreply, {MainIp, Parties}}
	end;

% all other messages
handle_cast(_Request, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, {_MainIp, Parties}) ->
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
	gen_server:cast({global, rtpproxy}, {call_terminated, {self(), Reason}}),
	?PRINT("terminated due to reason [~p]", [Reason]).

% rtp from some port
handle_info({udp, Fd, Ip, Port, Msg}, {MainIp, Parties}) ->
%	?PRINT("udp from Fd [~w] [~p:~p]", [Fd, Ip, Port]),
	Fun1 = fun (X,Y1,Y2,Z) ->
		case lists:keysearch(X,Y1,Z) of
			{value, Val1} ->
				{value, Y1, Val1};
			false ->
				case lists:keysearch (X,Y2,Z) of
					{value, Val2} ->
						{value, Y2, Val2};
					false ->
						false
				end
		end
	end,
	case Fun1(Fd, #party.fdfrom, #party.fdto, Parties) of
		{value, #party.fdto, Party} when Party#party.ipto /= null, Party#party.portto /= null, Party#party.pid == null, Party#party.answered == true ->
			% We don't need to check FdFrom for existence since it's no doubt exists
			{ok, PartyPid} = media:start({self(), {Party#party.fdfrom, Ip, Port}, {Party#party.fdto, Party#party.ipto, Party#party.portto}}),
			% FIXME send this Msg
			gen_udp:controlling_process(Party#party.fdfrom, PartyPid),
			gen_udp:controlling_process(Party#party.fdto, PartyPid),
			{noreply, {MainIp, lists:keyreplace(Fd, #party.fdto, Parties, Party#party{ipfrom=Ip, portfrom=Port, pid=PartyPid})}};
		{value, #party.fdto, Party} ->
			{noreply, {MainIp, lists:keyreplace(Fd, #party.fdto, Parties, Party#party{ipfrom=Ip, portfrom=Port})}};
		{value, #party.fdfrom, Party} when Party#party.ipfrom /= null, Party#party.portfrom /= null, Party#party.fdto /= null, Party#party.pid == null, Party#party.answered == true ->
			{ok, PartyPid} = media:start({self(), {Party#party.fdfrom, Party#party.ipfrom, Party#party.portfrom}, {Party#party.fdto, Ip, Port}}),
			% FIXME send this Msg
			gen_udp:controlling_process(Party#party.fdfrom, PartyPid),
			gen_udp:controlling_process(Party#party.fdto, PartyPid),
			{noreply, {MainIp, lists:keyreplace(Fd, #party.fdfrom, Parties, Party#party{ipto=Ip, portto=Port, pid=PartyPid})}};
		{value, #party.fdfrom, Party} ->
			% guessing that caller has uPnP
			case Party#party.fdto of
				null ->
					ok;
				_ ->
					gen_udp:send(Party#party.fdto, Party#party.ipfromguess, Party#party.portfromguess, Msg)
			end,
			{noreply, {MainIp, lists:keyreplace(Fd, #party.fdfrom, Parties, Party#party{ipto=Ip, portto=Port})}};
		false ->
			{noreply, {MainIp, Parties}}
	end;

handle_info(Info, State) ->
	?PRINT("Info [~w]", [Info]),
	{noreply, State}.
