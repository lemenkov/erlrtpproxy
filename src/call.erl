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
-include_lib("eradius/include/eradius_lib.hrl").
-include_lib("eradius/include/dictionary.hrl").
-include_lib("eradius/include/dictionary_cisco.hrl").

-record(source, {fd=null, ip=null, port=null, tag=null}).
-record(state, {ip, callid, parties=[], radius, tref, tref2, status=notstarted}).

-record(party, {from=null,
		fromrtcp=null,
		to=null,
		tortcp=null,
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

init (CallID) ->
	process_flag(trap_exit, true),
	{ok, RadAcctServers} = application:get_env(rtpproxy, radacct_servers),
	% TODO just choose first one for now
	[MainIp | _Rest ]  = get_ipaddrs(),
	eradius_dict:start(),
	eradius_dict:load_tables(["dictionary", "dictionary_cisco"]),
	case eradius_dict:lookup(?Acct_Session_Id) of
		[] ->
			?ERR("Can't load dictionary", []),
			{stop, "Can't load dictionary"};
		_ ->
			eradius_acc:start(),
			{ok, TRef} = timer:send_interval(?CALL_TIME_TO_LIVE*5, timeout),
			{ok, TRef2} = timer:send_interval(?CALL_TIME_TO_LIVE, interim_update),
			?INFO("started at ~s", [inet_parse:ntoa(MainIp)]),
			{ok, #state{	ip=MainIp,
					callid=CallID,
					radius=#rad_accreq{	servers=RadAcctServers,
								login_time = undefined,
								std_attrs=[{?Acct_Session_Id, CallID}]},
					tref=TRef,
					tref2=TRef2}}
	end.

% handle originate call leg (new media id)
% TODO handle Modifiers
handle_call({?CMD_U, {{GuessIp, GuessPort}, {FromTag, MediaId}, null, _Modifiers}}, _, #state{ip = Ip, parties = Parties} = State) ->
	?INFO("message [U] probably from ~w:~w  MediaId [~b]", [GuessIp, GuessPort, MediaId]),
	% search for already  existed
	case lists:keysearch(MediaId, #party.mediaid, Parties) of
		% call already exists
		{value, Party} ->
			case FromTag == (Party#party.from)#source.tag of
				true ->
					{ok, {LocalIp, LocalPort}} = inet:sockname((Party#party.from)#source.fd),
					Reply = integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
					?INFO("answer [~s] (already exists!)", [Reply]),
					{reply, {ok, old, Reply}, State};
				_ ->
					{reply, {error, not_found}, State}
			end;
		false ->
			% open new FdFrom and attach it
			case get_fd_quadruple(Ip) of
				{Fd0, Fd1, Fd2, Fd3} ->
					NewParty = #party{	from	=#source{fd=Fd0, ip=GuessIp, port=GuessPort, tag=FromTag},
								fromrtcp=#source{fd=Fd1},
								to	=#source{fd=Fd2},
								tortcp	=#source{fd=Fd3},
								mediaid=MediaId},
					{ok, {LocalIp, LocalPort}} = inet:sockname(Fd0),
					Reply = integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
					?INFO("answer [~s]", [Reply]),
					{reply, {ok, new, Reply}, State#state{parties=lists:append(Parties, [NewParty])}};
				error ->
					?ERR("Create new socket at ~p FAILED", [Ip]),
					{reply, {error, udp_error}, State}
			end
	end;

% REINVITE
% TODO handle Modifiers
handle_call({?CMD_U, {{GuessIp, GuessPort}, {FromTag, MediaId}, To, _Modifiers}}, _, #state{parties = Parties} = State) ->
	?INFO("message [U] probably from ~w:~w  MediaId [~b] REINVITE!", [GuessIp, GuessPort, MediaId]),
	case lists:keysearch(MediaId, #party.mediaid, Parties) of
		% call already exists
		{value, Party} ->
			Ret = if
				FromTag == (Party#party.from)#source.tag ->
					inet:sockname((Party#party.from)#source.fd);
				FromTag == (Party#party.to)#source.tag ->
					inet:sockname((Party#party.to)#source.fd);
				true -> {error, not_found}
			end,
			case Ret of
				{ok, {LocalIp, LocalPort}} ->
					Reply = integer_to_list(LocalPort) ++ " " ++ inet_parse:ntoa(LocalIp),
					?INFO("answer [~s] (already exists!)", [Reply]),
					{reply, {ok, old, Reply}, State};
				_ ->
					{reply, {error, not_found}, State}
			end;
		false ->
			{reply, {error, not_found}, State}
	end;

% handle answered call leg
% Both MediaId's are equal (just guessing)
handle_call({?CMD_L, {{GuessIp, GuessPort}, {FromTag, MediaId}, {ToTag, MediaId}, Modifiers}}, _, State) ->
	?INFO("message [L] probably from ~w:~w  MediaId [~b] with modifiers: ~p", [GuessIp, GuessPort, MediaId, Modifiers]),
	% search for already  existed
	case lists:keysearch(MediaId, #party.mediaid, State#state.parties) of
		{value, Party} ->
			case
				if
					FromTag == (Party#party.to)#source.tag ->
						{ok, (Party#party.from)#source.fd};
					FromTag == (Party#party.from)#source.tag ->
						{ok, (Party#party.to)#source.fd};
					true ->
						{error, not_found}
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
					% TODO use some other flag
					case lists:member(mod_i, Modifiers) of
						true ->
							case (State#state.radius)#rad_accreq.login_time of
								undefined ->
									Req = (State#state.radius)#rad_accreq{
											login_time = erlang:now(),
											vend_attrs = [{?Cisco, [{?h323_connect_time, date_time_fmt()}]}]},
									eradius_acc:acc_start(Req),
									{reply, {ok, Reply}, State#state{parties=lists:keyreplace(MediaId, #party.mediaid, State#state.parties, NewParty), radius=Req}};
								_ ->
									{reply, {ok, Reply}, State#state{parties=lists:keyreplace(MediaId, #party.mediaid, State#state.parties, NewParty)}}
							end;
						_ ->
							{reply, {ok, Reply}, State#state{parties=lists:keyreplace(MediaId, #party.mediaid, State#state.parties, NewParty)}}
					end;
				{error, not_found} ->
					?ERR("FAILED", []),
					{reply, {error, not_found}, State}
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
	{reply, Result, State}.

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
handle_cast({stop, Reason, Pid}, State) ->
%	?INFO("TIMEOUT when state is [~p]", [Parties]),
	case lists:keytake (Pid, #party.pid, State#state.parties) of
		{value, Party, []} ->
%			?INFO("It was the last mediastream - exiting", []),
			case Reason of
				timeout -> {stop, timeout, State#state{parties=[Party#party{pid=null}]}};
				_ ->  {stop, stop, State#state{parties=[Party#party{pid=null}]}}
			end;
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
					case Reason of
						timeout -> {stop, timeout, State#state{parties=NewParties++[Party#party{pid=null}]}};
						_ ->  {stop, stop, State#state{parties=NewParties++[Party#party{pid=null}]}}
					end
			end;
		false ->
			?ERR("Cannot find such Pid", []),
			{noreply, State}
	end.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, State) ->

	% if some media pids are still aive, then we just send message to them (media thread will close ports by himself)
	% if media pids == null, then we'll try to close ports by ourselves (it's possible that media thread wasn't even started)
	lists:foreach(
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
			end
		end,
		State#state.parties),

	% we'll send  (if login_time was defined before) 'STOP' radius accounting message
	case (State#state.radius)#rad_accreq.login_time of
		undefined ->
			ok;
		_ ->
			Req0 = eradius_acc:set_logout_time(State#state.radius),
			Req1 = Req0#rad_accreq{vend_attrs = [{?Cisco, [{?h323_disconnect_time, date_time_fmt()}]}]},
			eradius_acc:acc_stop(Req1)
	end,

	timer:cancel(State#state.tref),
	timer:cancel(State#state.tref2),

	% We'll notify rtpproxy about our termination
	gen_server:cast({global, rtpproxy}, {call_terminated, self(), Reason}),

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
		SafeGetAddr = fun(X) ->
			case X of
				null -> null;
				_ -> {X#source.fd, X#source.ip, X#source.port}
			end
		end,
		{ok, P} = media:start({self(), State#state.callid, {F#source.fd, F#source.ip, F#source.port}, SafeGetAddr(FRtcp), {T#source.fd, T#source.ip, T#source.port}, SafeGetAddr(TRtcp)}),
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
		{OldParty, NewParty1, Status} ->
			{noreply, State#state{status=Status, parties=lists:delete(OldParty, State#state.parties) ++ [NewParty1]}};
		false ->
%			?WARN("Probably RTCP to ~p from Ip[~p] Port[~p]", [Fd, Ip, Port]),
			{noreply, State}
	end;
handle_info(timeout, #state{status = notstarted} = State) ->
	{stop, timeout, State};

handle_info(timeout, State) when (State#state.radius)#rad_accreq.login_time /= undefined ->
	eradius_acc:acc_update(State#state.radius),
	{noreply, State};

handle_info(interim_update, State) when (State#state.radius)#rad_accreq.login_time /= undefined ->
	eradius_acc:acc_update(State#state.radius),
	{noreply, State};

handle_info(Info, State) ->
	?WARN("Info [~w]", [Info]),
	{noreply, State}.

%%
%% Private functions
%%

date_time_fmt() ->
	{{YYYY,MM,DD},{Hour,Min,Sec}} = erlang:localtime(),
%	DayNumber = calendar:day_of_the_week({YYYY,MM,DD}),
%	lists:flatten(io_lib:format("~s ~3s ~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w ~4.4.0w",[httpd_util:day(DayNumber),httpd_util:month(MM),DD,Hour,Min,Sec,YYYY])).
	lists:flatten(io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w", [YYYY, MM, DD, Hour,Min,Sec])).

get_ipaddrs() ->
	% TODO IPv4 only
	{ok, IPv4List} = inet:getif(),
	FilterIP = fun (F) ->
			fun
				({[], X}) ->
					X;
				% Loopback
				({[{{127, _, _, _}, _Bcast,_Mask} | Rest], X}) ->
					F({Rest, X});
				% RFC 1918, 10.0.0.0 - 10.255.255.255
				({[{{10,_,_,_}, _Bcast,_Mask} | Rest], X}) ->
					F({Rest, X});
				% RFC 1918, 172.16.0.0 - 172.31.255.255
				({[{{172, A ,_,_}, _Bcast,_Mask} | Rest], X}) when A > 15 , A < 32 ->
					F({Rest, X});
				% RFC 1918, 192.168.0.0 - 192.168.255.255
				({[{{192, 168,_,_}, _Bcast,_Mask} | Rest], X}) ->
					F({Rest, X});
				({[ {IPv4, _Bcast,_Mask} | Rest], X}) ->
					F({Rest, X ++ [IPv4]})
			end
	end,
	(y:y(FilterIP))({IPv4List, []}).

get_fd_pair(Ip) ->
	get_fd_pair(Ip, 10).
get_fd_pair(Ip, 0) ->
	?ERR("Create new socket at ~p FAILED", [Ip]),
	error;
get_fd_pair(Ip, NTry) ->
	case gen_udp:open(0, [binary, {ip, Ip}, {active, true}, {raw,1,11,<<1:32/native>>}]) of
		{ok, Fd} ->
			{ok, {Ip,Port}} = inet:sockname(Fd),
			Port2 = case Port rem 2 of
				0 -> Port + 1;
				1 -> Port - 1
			end,
			case gen_udp:open(Port2, [binary, {ip, Ip}, {active, true}, {raw,1,11,<<1:32/native>>}]) of
				{ok, Fd2} ->
					if
						Port > Port2 -> {Fd2, Fd};
						Port < Port2 -> {Fd, Fd2}
					end;
				{error, _} ->
					gen_udp:close(Fd),
					get_fd_pair(Ip, NTry - 1)
			end;
		{error, _} ->
			get_fd_pair(Ip, NTry - 1)
	end.

get_fd_quadruple(Ip) ->
	case get_fd_pair(Ip) of
		{Fd0, Fd1} ->
			case get_fd_pair(Ip) of
				{Fd2, Fd3} ->
					{Fd0, Fd1, Fd2, Fd3};
				error ->
					gen_udp:close(Fd0),
					gen_udp:close(Fd1),
					error
			end;
		error -> error
	end.
