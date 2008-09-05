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

-record(source, {fd=null, ip=null, port=null}).

-record(party, {from=null,
		to=null,
		mediaid=null,
		pid=null,
		answered=false,
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
handle_call({message_u, {{GuessIp, GuessPort}, {_FromTag, MediaId}, Modifiers}}, _From, {MainIp, Parties}) ->
	?PRINT ("message [U] MediaId [~b]", [MediaId]),
	% search for already  existed
	case lists:keysearch(MediaId, #party.mediaid, Parties) of
		% call already exists
		{value, Party} ->
			?PRINT("Already exists!", []),
			{ok, {LocalIp, LocalPort}} = inet:sockname((Party#party.from)#source.fd),
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

					NewParty = #party{from=#source{fd=Fd1, ip=GuessIp, port=GuessPort}, to=#source{fd=Fd2}, mediaid=MediaId},

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
handle_call({message_l, {{_FromTag, MediaId}, {_ToTag, MediaId}, Modifiers}}, _From, {MainIp, Parties}) ->
	?PRINT ("message [L] MediaId [~b]", [MediaId]),
	% search for already  existed
	case lists:keysearch(MediaId, #party.mediaid, Parties) of
		% call already exists and Fd is not opened
		{value, Party} when (Party#party.to)#source.fd == null ->
			?PRINT("Already exists (To.Fd == NULL)!", []),
			case gen_udp:open(0, [binary, {ip, MainIp}, {active, true}]) of
				{ok, Fd} ->
					{ok, {LocalIp, LocalPort}} = inet:sockname(Fd),
					NewParty = Party#party{to=(Party#party.to)#source{fd=Fd}, answered=true},

					Reply = integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
%					?PRINT("answer [~s]", [Reply]),
					{reply, {ok, Reply}, {MainIp, lists:keyreplace(MediaId, #party.mediaid, Parties, NewParty)}};
				{error, Reason} ->
					?PRINT("FAILED [~p]", [Reason]),
					{reply, {error, udp_error}, {MainIp, Parties}}
			end;
		% call already exists and Fd is opened
		{value, Party} when (Party#party.to)#source.fd /= null ->
			?PRINT("Already exists (To.Fd != NULL)!", []),

			{ok, {LocalIp, LocalPort}} = inet:sockname((Party#party.to)#source.fd),
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

handle_call(message_p, _From, {MainIp, Parties}) ->
	?PRINT("Message [P] [~p]", [Parties]),
	Fun = fun(F) ->
		fun	({[], Result}) ->
				Result;
			({[Party|Rest], Result}) ->
				case Party#party.pid of
					null ->
						F ({Rest, Result});
					_ ->
						gen_server:cast(Party#party.pid, hold),
						F ({Rest, Result ++ [{{(Party#party.to)#source.fd, (Party#party.from)#source.ip, (Party#party.from)#source.port},
									{(Party#party.from)#source.fd, (Party#party.to)#source.ip, (Party#party.to)#source.port}}]})
				end
		end
	end,
	Result = (y(Fun))({Parties, []}),
	{reply, {ok, Result}, {MainIp, Parties}};


handle_call(_Other, _From, State) ->
	{noreply, State}.

handle_cast(message_d, State) ->
	{stop, message_d, State};

handle_cast({message_r, filename}, State) ->
	% TODO start recording of RTP
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
	Fun = fun (F) ->
		fun	({[], X}) ->
				false;
			({[Elem|Rest], X}) ->
				case (Elem#party.from)#source.fd of
					X ->
						{value, from, Elem};
					_ ->
						case (Elem#party.to)#source.fd of
							X ->
								{value, to, Elem};
							_ ->
								F({Rest, X})
						end
				end
		end
	end,
	case
		case (y(Fun))({Parties, Fd}) of
			% RTP to Callee
			{value, to, Party} when
						(Party#party.to)#source.ip /= null,
						(Party#party.to)#source.port /= null,
						% We don't need to check FdFrom for existence since it's no doubt exists
						% (Party#party.from)#source.fd /= null,
						Party#party.pid == null,
						Party#party.answered == true
							->
				{ok, PartyPid} = media:start({self(), {(Party#party.from)#source.fd, Ip, Port}, {(Party#party.to)#source.fd, (Party#party.to)#source.ip, (Party#party.to)#source.port}}),
				% FIXME send Msg in this case
				gen_udp:controlling_process((Party#party.from)#source.fd, PartyPid),
				gen_udp:controlling_process((Party#party.to)#source.fd,   PartyPid),
				{Party, Party#party{from=(Party#party.from)#source{ip=Ip, port=Port}, pid=PartyPid}};
			% RTP to Caller
			{value, from, Party} when
						(Party#party.from)#source.ip /= null,
						(Party#party.from)#source.port /= null,
						(Party#party.to)#source.fd /= null,
						Party#party.pid == null,
						Party#party.answered == true
							->
				{ok, PartyPid} = media:start({self(), {(Party#party.from)#source.fd, (Party#party.from)#source.ip, (Party#party.from)#source.port}, {(Party#party.to)#source.fd, Ip, Port}}),
				% FIXME send Msg in this case
				gen_udp:controlling_process((Party#party.from)#source.fd, PartyPid),
				gen_udp:controlling_process((Party#party.to)#source.fd, PartyPid),
				{Party, Party#party{to=(Party#party.to)#source{ip=Ip, port=Port}, pid=PartyPid}};
			% RTP to Caller
			{value, from, Party} ->
				% TODO guess that caller has uPnP
				{Party, Party#party{to=(Party#party.to)#source{ip=Ip, port=Port}}};
			% RTP to Callee
			{value, to, Party} ->
				{Party, Party#party{from=(Party#party.from)#source{ip=Ip, port=Port}}};
			false ->
				false
		end
	of
		{OldParty, NewParty} ->
			{noreply, {MainIp, lists:delete(OldParty, Parties) ++ [NewParty]}};
		false ->
			{noreply, {MainIp, Parties}}
	end;

handle_info(Info, State) ->
	?PRINT("Info [~w]", [Info]),
	{noreply, State}.

y(M) ->
	G = fun (F) -> M(fun(A) -> (F(F))(A) end) end,
	G(G).

