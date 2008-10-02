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

-record(source, {fd=null, ip=null, port=null, tag=null}).

-record(party, {from=null,
		fromrtcp=null,
		to=null,
		tortcp=null,
		startport=0,
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
handle_call({message_u, {StartPort, {GuessIp, GuessPort}, {FromTag, MediaId}, To, Modifiers}}, _, {MainIp, Parties}) ->
	case To of
		null ->
			?PRINT ("message [U] probably from ~w:~w  MediaId [~b]", [GuessIp, GuessPort, MediaId]);
		_ ->
			?PRINT ("message [U] probably from ~w:~w  MediaId [~b] REINVITE!", [GuessIp, GuessPort, MediaId])
	end,
	% search for already  existed
	case lists:keysearch(MediaId, #party.mediaid, Parties) of
		% call already exists
		{value, Party} ->
			{ok, {LocalIp, LocalPort}} = if
				FromTag == (Party#party.from)#source.tag -> inet:sockname((Party#party.from)#source.fd);
				FromTag == (Party#party.to)#source.tag   -> inet:sockname((Party#party.to)#source.fd)
			end,
			Reply = integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
			?PRINT("answer [~s] (already exists!)", [Reply]),
			{reply, {ok, old, Reply}, {MainIp, Parties}};
		false ->
			% open new FdFrom and attach it
			case gen_udp:open(StartPort, [binary, {ip, MainIp}, {active, true}]) of
				{ok, Fd} ->
					SafeOpenFd = fun(Port, Params) when is_list (Params) ->
						case gen_udp:open(Port, Params) of
							{ok, F} -> F;
							_ -> null
						end
					end,
					NewParty = #party{	from	=#source{fd=Fd, ip=GuessIp, port=GuessPort, tag=FromTag},
								fromrtcp=#source{fd=SafeOpenFd (StartPort+1, [binary, {ip, MainIp}, {active, true}])},
								to	=#source{fd=SafeOpenFd (StartPort+2, [binary, {ip, MainIp}, {active, true}])},
								tortcp	=#source{fd=SafeOpenFd (StartPort+3, [binary, {ip, MainIp}, {active, true}])},
								startport=StartPort,
								mediaid=MediaId},
					{ok, {LocalIp, LocalPort}} = inet:sockname(Fd),
					Reply = integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
					?PRINT("answer [~s]", [Reply]),
					{reply, {ok, new, Reply}, {MainIp, lists:append(Parties, [NewParty])}};
				{error, Reason} ->
					?PRINT("Create new socket FAILED [~p]", [Reason]),
					{reply, {error, udp_error}, {MainIp, Parties}}
			end
	end;

% handle answered call leg
% Both MediaId's are equal (just guessing)
handle_call({message_l, {{GuessIp, GuessPort}, {FromTag, MediaId}, {ToTag, MediaId}, Modifiers}}, _, {MainIp, Parties}) ->
	?PRINT ("message [L] probably from ~w:~w  MediaId [~b]", [GuessIp, GuessPort, MediaId]),
	% search for already  existed
	case lists:keysearch(MediaId, #party.mediaid, Parties) of
		{value, Party} ->
			case
				case (Party#party.to)#source.fd of
					null ->
						gen_udp:open(Party#party.startport+2, [binary, {ip, MainIp}, {active, true}]);
					_ ->
						try
							if
								FromTag == (Party#party.to)#source.tag -> {ok, (Party#party.from)#source.fd};
								FromTag == (Party#party.from)#source.tag   -> {ok, (Party#party.to)#source.fd}
							end
						catch
							Exception:ExceptionClass -> {Exception, ExceptionClass}
						end
				end
			of
				{ok, Fd} ->
					NewParty = case (Party#party.to)#source.tag of
						null -> Party#party{to=#source{fd=Fd, ip=GuessIp, port=GuessPort, tag=ToTag}, answered=true};
						_ -> Party
					end,
					{ok, {LocalIp, LocalPort}} = inet:sockname(Fd),
					Reply = integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
					?PRINT("answer [~s]", [Reply]),
					{reply, {ok, Reply}, {MainIp, lists:keyreplace(MediaId, #party.mediaid, Parties, NewParty)}};
				{error, Reason} ->
					?PRINT("FAILED [~p]", [Reason]),
					{reply, {error, udp_error}, {MainIp, Parties}}
			end;
		false ->
			% Call not found.
			?PRINT("ERROR not found", []),
			{reply, {error, not_found}, {MainIp, Parties}}
	end;

handle_call(message_i, _From, State) ->
	% TODO (acquire information about call state)
	{reply, {ok, "TODO"}, State};

handle_call({message_p, {Tag, MediaId}}, _From, {MainIp, Parties}) ->
	?PRINT("Message [P] [~p]", [Parties]),
	case lists:keysearch(MediaId, #party.mediaid, Parties) of
		% call already exists
		{value, Party} ->
			gen_server:cast(Party#party.pid, hold),
			Result = if
				Tag == (Party#party.from)#source.tag -> {(Party#party.to)#source.fd, (Party#party.from)#source.ip, (Party#party.from)#source.port};
				Tag == (Party#party.to)#source.tag   -> {(Party#party.from)#source.fd, (Party#party.to)#source.ip, (Party#party.to)#source.port}
			end,
			{reply, {ok, Result}, {MainIp, Parties}};
		false ->
			{reply, {error, not_found}, {MainIp, Parties}}
	end;


handle_call(_Other, _From, State) ->
	{noreply, State}.

handle_cast(message_d, State) ->
	% No need to cleanup  list of media-streams here
	% we'll do it later, at terminate(...)
	{stop, message_d, State};

handle_cast({message_r, Filename}, {MainIp, Parties}) ->
	_Unused = lists:foreach(
		fun(X)  ->
			if
				X#party.pid /= null ->
					gen_server:cast(X#party.pid, {recording, {start, Filename}});
				true ->
					ok
			end
		end,
		Parties),
	{noreply, {MainIp, Parties}};

handle_cast(message_s, {MainIp, Parties}) ->
	_Unused = lists:foreach(
		fun(X)  ->
			if
				X#party.pid /= null ->
					gen_server:cast(X#party.pid, {recording, stop});
				true ->
					ok
			end
		end,
		Parties),
	{noreply, {MainIp, Parties}};

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
	Ports = lists:map(
		fun(X)  ->
			if
				X#party.pid /= null ->
					gen_server:cast(X#party.pid, stop);
				true ->
					ok
			end,
			X#party.startport
		end,
		Parties),
	gen_server:cast({global, rtpproxy}, {call_terminated, {self(), {ports, Ports}, Reason}}),
	?PRINT("terminated due to reason [~p]", [Reason]).

% rtp from some port
handle_info({udp, Fd, Ip, Port, Msg}, {MainIp, Parties}) ->
%	?PRINT("udp from Fd [~w] [~p:~p]", [Fd, Ip, Port]),
	FindFd = fun (F) ->
		fun	({[], _X}) ->
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
	SafeStart = fun (F,FRtcp,T,TRtcp) ->
		SafeGetAddr = fun(X) ->
			case X of
				null -> null;
				_ -> {X#source.fd, X#source.ip, X#source.port}
			end
		end,
		{ok, P} = media:start({self(), {F#source.fd, F#source.ip, F#source.port}, SafeGetAddr(FRtcp), {T#source.fd, T#source.ip, T#source.port}, SafeGetAddr(TRtcp)}),
		gen_udp:controlling_process(F#source.fd, P),
		gen_udp:controlling_process(T#source.fd, P),
		lists:foreach(fun(X) -> case X of null -> ok; _ -> gen_udp:controlling_process(X#source.fd, P) end end, [FRtcp, FRtcp]),
		P
	end,
	case
		case (utils:y(FindFd))({Parties, Fd}) of
			% RTP from Caller to Callee
			{value, to, Party} when
						(Party#party.to)#source.ip /= null,
						(Party#party.to)#source.port /= null,
						% We don't need to check FdFrom for existence since it's no doubt exists
						% (Party#party.from)#source.fd /= null,
						Party#party.pid == null,
						Party#party.answered == true
							->
				NewParty = Party#party{from=(Party#party.from)#source{ip=Ip, port=Port}},
				% FIXME send Msg here - we created Media server and we need to pass Msg to him
				{Party, NewParty#party{pid=SafeStart(NewParty#party.from, NewParty#party.fromrtcp, NewParty#party.to, NewParty#party.tortcp)}};
			% RTP to Caller from Callee
			{value, from, Party} when
						(Party#party.from)#source.ip /= null,
						(Party#party.from)#source.port /= null,
						(Party#party.to)#source.fd /= null,
						Party#party.pid == null,
						Party#party.answered == true
							->
				NewParty = Party#party{to=(Party#party.to)#source{ip=Ip, port=Port}},
				% FIXME send Msg here - we created Media server and we need to pass Msg to him
				{Party, NewParty#party{pid=SafeStart(NewParty#party.from, NewParty#party.fromrtcp, NewParty#party.to, NewParty#party.tortcp)}};
			% RTP to Caller from Callee
			{value, from, Party} ->
				% TODO guess that Caller has uPnP - we know Ip and Port for Callee, and we got GuessIp and GuessPort for Caller
				%      so we should try to start this session here (in any case 'мы ничем не рискуем')
				{Party, Party#party{to=(Party#party.to)#source{ip=Ip, port=Port}}};
			% RTP from Caller to Callee
			{value, to, Party} ->
				% we should dismiss this Msg since we don't know all necessary data about Callee
				{Party, Party#party{from=(Party#party.from)#source{ip=Ip, port=Port}}};
			false ->
				false
		end
	of
		{OldParty, NewParty1} ->
			{noreply, {MainIp, lists:delete(OldParty, Parties) ++ [NewParty1]}};
		false ->
			?PRINT("Probably RTCP to ~p from Ip[~p] Port[~p]", [Fd, Ip, Port]),
			{noreply, {MainIp, Parties}}
	end;

handle_info(Info, State) ->
	?PRINT("Info [~w]", [Info]),
	{noreply, State}.

