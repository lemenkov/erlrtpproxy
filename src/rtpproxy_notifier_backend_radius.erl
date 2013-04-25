-module(rtpproxy_notifier_backend_radius).

-behaviour(gen_server).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include_lib("eradius/include/eradius_lib.hrl").
-include_lib("eradius/include/dictionary_cisco.hrl").
-include_lib("eradius/include/dictionary_rfc2866.hrl").

-record(state, {
	tref,
	req
}).

init([C, _M]) ->
	process_flag(trap_exit, true),
	{ok, Timeout} = application:get_env(rtpproxy, ttl),
	{ok, TRef} = timer:send_interval(Timeout*1000, interim_update),
	eradius_dict:start(),
	eradius_dict:load_tables(["dictionary", "dictionary_cisco", "dictionary_rfc2865", "dictionary_rfc2866"]),
	eradius_acc:start(),
	{ok, RadAcctServers} = application:get_env(rtpproxy, radacct_servers),
	Req = #rad_accreq{
		servers = RadAcctServers,
		login_time = os:timestamp(),
		std_attrs=[{?Acct_Session_Id, C}],
		vend_attrs = [{?Cisco, [{?h323_connect_time, date_time_fmt()}]}]
	},
	eradius_acc:acc_start(Req),
	error_logger:info_msg("RADIUS notify backend: started at ~p with CallID: ~s~n", [node(), C]),
	{ok, #state{tref = TRef, req = Req}}.

handle_call(Call, _From, State) ->
	error_logger:error_msg("RADIUS notify backend: strange call: ~p~n", [Call]),
	{stop, {error, {unknown_call, Call}}, State}.

handle_cast(Cast, State) ->
	error_logger:error_msg("RADIUS notify backend: strange cast: ~p~n", [Cast]),
	{stop, {error, {unknown_cast, Cast}}, State}.

handle_info(interim_update, #state{req = Req} = State) ->
	CurrTime = os:timestamp(),
	LoginTime = to_now(Req#rad_accreq.login_time),
	SessTime = to_seconds(CurrTime) - to_seconds(LoginTime),
	eradius_acc:acc_update(Req#rad_accreq{session_time = SessTime}),
	error_logger:info_msg("RADIUS notify backend: interim_update~n"),
	{noreply, State};

handle_info(Info, State) ->
	error_logger:error_msg("RADIUS notify backend: strange info: ~p~n", [Info]),
	{stop, {error, {unknown_info, Info}}, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, #state{tref = TRef, req = Req0}) ->
	Req1 = eradius_acc:set_logout_time(Req0),
	Req2 = Req1#rad_accreq{
		vend_attrs = [{?Cisco, [{?h323_disconnect_time, date_time_fmt()}]}]
	},
	eradius_acc:acc_stop(Req2),
	{memory, Bytes} = erlang:process_info(self(), memory),
	timer:cancel(TRef),
	error_logger:info_msg("RADIUS notify backend: terminated due to reason [~p] (allocated ~b bytes)", [Reason, Bytes]).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions %%
%%%%%%%%%%%%%%%%%%%%%%%%

date_time_fmt() ->
	{{YYYY,MM,DD},{Hour,Min,Sec}} = erlang:localtime(),
	% DayNumber = calendar:day_of_the_week({YYYY,MM,DD}),
	% lists:flatten(io_lib:format("~s ~3s ~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w ~4.4.0w",[httpd_util:day(DayNumber),httpd_util:month(MM),DD,Hour,Min,Sec,YYYY])).
	lists:flatten(io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w", [YYYY, MM, DD, Hour,Min,Sec])).

to_now(Now = {MSec, Sec, USec}) when is_integer(MSec), is_integer(Sec), is_integer(USec) ->
	Now;
to_now(Now) when is_integer(Now) ->
	{Now div 1000000, Now rem 1000000, 0}.

to_seconds(Now = {MSec, Sec, USec}) when is_integer(MSec), is_integer(Sec), is_integer(USec) ->
	calendar:datetime_to_gregorian_seconds(calendar:now_to_local_time(Now));
to_seconds(Seconds) when is_integer(Seconds) ->
	Seconds.
