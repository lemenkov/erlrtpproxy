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

-include("../include/common.hrl").
-include("config.hrl").
-include_lib("eradius/src/eradius_lib.hrl").

-record(source, {fd=null, ip=null, port=null, tag=null}).
-record(state, {ip, callid, parties=[], radius, tref}).

-record(party, {from=null,
		fromrtcp=null,
		to=null,
		tortcp=null,
		startport=0,
		mediaid=null,
		pid=null,
		mod_e=false,
		mod_i=false,
		mod_ipv6=false,
		mod_symmetric=true,
		mod_weak=false,
		mod_z=false}).

start(Args) ->
	gen_server:start(?MODULE, Args, []).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

init ({CallID, MainIp, RadAcctServers}) ->
	process_flag(trap_exit, true),
	% TODO we should add app-file for eradius
	eradius_acc:start(),
	{ok, TRef} = timer:send_interval(?CALL_TIME_TO_LIVE, timeout),
	?INFO("started at ~s", [inet_parse:ntoa(MainIp)]),
	{ok, #state{ip=MainIp, callid=CallID, radius=#rad_accreq{servers=RadAcctServers}, tref=TRef}}.

% handle originate call leg (new media id possibly)
% TODO handle Modifiers
handle_call({?CMD_U, {StartPort, {GuessIp, GuessPort}, {FromTag, MediaId}, To, Modifiers}}, _, State) ->
	case To of
		null ->
			?INFO("message [U] probably from ~w:~w  MediaId [~b]", [GuessIp, GuessPort, MediaId]);
		_ ->
			?INFO("message [U] probably from ~w:~w  MediaId [~b] REINVITE!", [GuessIp, GuessPort, MediaId])
	end,
	% search for already  existed
	case lists:keysearch(MediaId, #party.mediaid, State#state.parties) of
		% call already exists
		{value, Party} ->
			case
				try
					if
						FromTag == (Party#party.from)#source.tag -> inet:sockname((Party#party.from)#source.fd);
						FromTag == (Party#party.to)#source.tag   -> inet:sockname((Party#party.to)#source.fd)
					end
				catch
					_:_ ->
						% TODO probably re-invite?
						{error, not_found}
				end
			of
				{ok, {LocalIp, LocalPort}} ->
					Reply = integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
					?INFO("answer [~s] (already exists!)", [Reply]),
					{reply, {ok, old, Reply}, State};
				{error, not_found} ->
					{reply, {error, not_found}, State}
			end;
		false ->
			% open new FdFrom and attach it
			case gen_udp:open(StartPort, [binary, {ip, State#state.ip}, {active, true}]) of
				{ok, Fd} ->
					SafeOpenFd = fun(Port, Params) when is_list (Params) ->
						case gen_udp:open(Port, Params) of
							{ok, F} -> F;
							_ -> null
						end
					end,
					NewParty = #party{	% TODO add Ip and Port only on demand
								from	=#source{fd=Fd, tag=FromTag},
%								from	=#source{fd=Fd, ip=GuessIp, port=GuessPort, tag=FromTag},
								fromrtcp=#source{fd=SafeOpenFd (StartPort+1, [binary, {ip, State#state.ip}, {active, true}])},
								to	=#source{fd=SafeOpenFd (StartPort+2, [binary, {ip, State#state.ip}, {active, true}])},
								tortcp	=#source{fd=SafeOpenFd (StartPort+3, [binary, {ip, State#state.ip}, {active, true}])},
								startport=StartPort,
								mediaid=MediaId},
					{ok, {LocalIp, LocalPort}} = inet:sockname(Fd),
					Reply = integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
					?INFO("answer [~s]", [Reply]),
					{reply, {ok, new, Reply}, State#state{parties=lists:append(State#state.parties, [NewParty])}};
				{error, Reason} ->
					?ERR("Create new socket at ~p ~p FAILED [~p]", [State#state.ip, StartPort, Reason]),
					{reply, {error, udp_error}, State}
			end
	end;

% handle answered call leg
% Both MediaId's are equal (just guessing)
handle_call({?CMD_L, {{GuessIp, GuessPort}, {FromTag, MediaId}, {ToTag, MediaId}, Modifiers}}, _, State) ->
	?INFO("message [L] probably from ~w:~w  MediaId [~b]", [GuessIp, GuessPort, MediaId]),
	% search for already  existed
	case lists:keysearch(MediaId, #party.mediaid, State#state.parties) of
		{value, Party} ->
			case
				case (Party#party.to)#source.fd of
					null ->
						gen_udp:open(Party#party.startport+2, [binary, {ip, State#state.ip}, {active, true}]);
					_ ->
						try
							if
								FromTag == (Party#party.to)#source.tag -> {ok, (Party#party.from)#source.fd};
								FromTag == (Party#party.from)#source.tag -> {ok, (Party#party.to)#source.fd}
							end
						catch
							Exception:ExceptionClass -> {Exception, ExceptionClass}
						end
				end
			of
				{ok, Fd} ->
					NewParty = case (Party#party.to)#source.tag of
						null -> Party#party{to=#source{fd=Fd, ip=GuessIp, port=GuessPort, tag=ToTag}};
						_ -> Party
					end,
					{ok, {LocalIp, LocalPort}} = inet:sockname(Fd),
					Reply = integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
					?INFO("answer [~s]", [Reply]),
					{reply, {ok, Reply}, State#state{parties=lists:keyreplace(MediaId, #party.mediaid, State#state.parties, NewParty)}};
				{error, Reason} ->
					?ERR("FAILED [~p]", [Reason]),
					{reply, {error, udp_error}, State}
			end;
		false ->
			% Call not found.
			?ERR("ERROR not found", []),
			{reply, {error, not_found}, State}
	end;

handle_call(?CMD_I, _From, State) ->
	PrintSource = fun(S) ->
		io_lib:format("Rtp: ~s:~p Tag:~s", [case S#source.ip of null -> "null"; _ -> inet_parse:ntoa(S#source.ip) end, S#source.port, S#source.tag])
	end,
	Reply = lists:map(fun (X) ->
				case X#party.pid of
					null -> ["CallDuration: not started yet"] ++ lists:map(fun(Y) -> PrintSource(Y) end, [X#party.from, X#party.fromrtcp, X#party.to, X#party.tortcp]);
					_ -> {ok, R} = gen_server:call(X#party.pid, ?CMD_I), [R]
				end
			end,
			State#state.parties),
	{reply, {ok, Reply}, State};

handle_call({?CMD_P, {Tag, MediaId}}, _From, State) ->
%	?INFO("Message [P] [~p]", [Parties]),
	Result = case lists:keysearch(MediaId, #party.mediaid, State#state.parties) of
		% call already exists
		{value, Party} ->
			gen_server:cast(Party#party.pid, hold),
			try
				if
					Tag == (Party#party.from)#source.tag ->
						{ok, {(Party#party.to)#source.fd, (Party#party.from)#source.ip, (Party#party.from)#source.port}};
					Tag == (Party#party.to)#source.tag   ->
						{ok, {(Party#party.from)#source.fd, (Party#party.to)#source.ip, (Party#party.to)#source.port}}
				end
			catch
				_:_ ->
					{error, not_found}
			end;
		false ->
			{error, not_found}
	end,
	{reply, Result, State};


handle_call(_Other, _From, State) ->
	{noreply, State}.

handle_cast(?CMD_D, State) ->
	% No need to cleanup  list of media-streams here
	% we'll do it later, at terminate(...)
	{stop, ?CMD_D, State};

handle_cast({?CMD_R, Filename}, State) ->
	[gen_server:cast(X#party.pid, {recording, {start, Filename}}) || X <- State#state.parties, X#party.pid /= null],
	{noreply, State};

handle_cast(?CMD_S, State) ->
	[gen_server:cast(X#party.pid, {recording, stop}) || X <- State#state.parties, X#party.pid /= null],
	{noreply, State};

% timeout from media stream
% TODO consider to stop all other media streams
handle_cast({stop, Pid}, State) ->
%	?INFO("TIMEOUT when state is [~p]", [Parties]),
	case lists:keytake (Pid, #party.pid, State#state.parties) of
		{value, Party, []} ->
%			?INFO("It was the last mediastream - exiting", []),
			{stop, stop, State#state{parties=[Party#party{pid=null}]}};
		{value, Party, NewParties} ->
%			?INFO("It was NOT the last mediastream", []),
			AlivePids = fun(X) ->
				case X#party.pid of
					null ->
						false;
					_ ->
						true
				end
			end,
			case lists:any(AlivePids, NewParties) of
				true ->
					{noreply, State#state{parties=NewParties++[Party#party{pid=null}]}};
				_ ->
					{stop, stop, State#state{parties=NewParties++[Party#party{pid=null}]}}
			end;
		false ->
			?ERR("Cannot find such Pid", []),
			{noreply, State}
	end;

% all other messages
handle_cast(_Request, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, State) ->
	Ports = lists:map(
		fun(X)  ->
			if
				X#party.pid /= null ->
					gen_server:cast(X#party.pid, stop);
				X#party.pid == null ->
					lists:foreach(
						fun(Y) ->
							try gen_udp:close(Y#source.fd)
							catch
								_:_ ->
									ok
							end
						end, [X#party.from, X#party.fromrtcp, X#party.to, X#party.tortcp])
			end,
			X#party.startport
		end,
		State#state.parties),
	case Reason of
		timeout ->
			ok;
		_ ->
			case (State#state.radius)#rad_accreq.login_time of
				undefined ->
					ok;
				_ ->
					Req = eradius_acc:set_logout_time(State#state.radius),
					eradius_acc:acc_stop(Req)
			end
	end,
	timer:cancel(State#state.tref),
	gen_server:cast({global, rtpproxy}, {call_terminated, {self(), {ports, Ports}, Reason}}),
	?ERR("terminated due to reason [~p]", [Reason]).

% rtp from some port
handle_info({udp, Fd, Ip, Port, Msg}, State) ->
%	?INFO("udp from Fd [~w] [~p:~p]", [Fd, Ip, Port]),
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
	SafeStart = fun (NP) ->
		[F,FRtcp,T,TRtcp] = [NP#party.from, NP#party.fromrtcp, NP#party.to, NP#party.tortcp],
		timer:cancel(State#state.tref),
		SafeGetAddr = fun(X) ->
			case X of
				null -> null;
				_ -> {X#source.fd, X#source.ip, X#source.port}
			end
		end,
		{ok, P} = media:start({self(), {F#source.fd, F#source.ip, F#source.port}, SafeGetAddr(FRtcp), {T#source.fd, T#source.ip, T#source.port}, SafeGetAddr(TRtcp)}),
		[gen_udp:controlling_process(X#source.fd, P) || X <- [F, FRtcp, T, TRtcp], X /= null],
		P
	end,
	case
		case (y:y(FindFd))({State#state.parties, Fd}) of
			% RTP from Caller to Callee
			{value, to, Party} when
						(Party#party.to)#source.ip /= null,
						(Party#party.to)#source.port /= null,
						% We don't need to check FdFrom for existence since it's no doubt exists
						% (Party#party.from)#source.fd /= null,
						Party#party.pid == null
							->
				NewParty = Party#party{from=(Party#party.from)#source{ip=Ip, port=Port}},
				% FIXME send Msg here - we created Media server and we need to pass Msg to him
				{Party, NewParty#party{pid=SafeStart(NewParty)}, started};
			% RTP to Caller from Callee
			{value, from, Party} when
						(Party#party.from)#source.ip /= null,
						(Party#party.from)#source.port /= null,
						(Party#party.to)#source.fd /= null,
						Party#party.pid == null
							->
				NewParty = Party#party{to=(Party#party.to)#source{ip=Ip, port=Port}},
				% FIXME send Msg here - we created Media server and we need to pass Msg to him
				{Party, NewParty#party{pid=SafeStart(NewParty)}, started};
			% RTP to Caller from Callee
			{value, from, Party} ->
				% TODO guess that Caller has uPnP - we know Ip and Port for Callee, and we got GuessIp and GuessPort for Caller
				%      so we should try to start this session here (in any case 'мы ничем не рискуем')
				{Party, Party#party{to=(Party#party.to)#source{ip=Ip, port=Port}}, notstarted};
			% RTP from Caller to Callee
			{value, to, Party} ->
				% we should dismiss this Msg since we don't know all necessary data about Callee
				{Party, Party#party{from=(Party#party.from)#source{ip=Ip, port=Port}}, notstarted};
			false ->
				false
		end
	of
		{OldParty, NewParty1, started} ->
			Req = eradius_acc:set_login_time(State#state.radius),
			eradius_acc:acc_start(Req),
			{noreply, State#state{radius=Req, parties=lists:delete(OldParty, State#state.parties) ++ [NewParty1]}};
		{OldParty, NewParty1, notstarted} ->
			{noreply, State#state{parties=lists:delete(OldParty, State#state.parties) ++ [NewParty1]}};
		false ->
%			?WARN("Probably RTCP to ~p from Ip[~p] Port[~p]", [Fd, Ip, Port]),
			{noreply, State}
	end;
handle_info(timeout, State) ->
	{stop, timeout, State};

handle_info(Info, State) ->
	?WARN("Info [~w]", [Info]),
	{noreply, State}.

