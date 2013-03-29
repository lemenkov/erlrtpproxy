-module(rtpproxy_notifier_backend_radius).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include_lib("eradius/include/eradius_lib.hrl").
-include_lib("eradius/include/dictionary_cisco.hrl").
-include_lib("eradius/include/dictionary_rfc2866.hrl").

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
	process_flag(trap_exit, true),
	eradius_dict:start(),
	eradius_dict:load_tables(["dictionary", "dictionary_cisco", "dictionary_rfc2865", "dictionary_rfc2866"]),
	eradius_acc:start(),
	?MODULE = ets:new(?MODULE, [public, named_table]),
	error_logger:info_msg("RADIUS notify backend: ~p - started at ~p~n", [self(), node()]),
	{ok, []}.

handle_call(Call, _From, State) ->
	error_logger:error_msg("RADIUS notify backend: ~p - strange call: ~p~n", [self(), Call]),
	{stop, {error, {unknown_call, Call}}, State}.

handle_cast({Type, CallId, MediaId, _}, State) ->
	error_logger:info_msg("Got ~p from ~s ~p at ~p~n", [Type, CallId, MediaId, node()]),
	radius_process(Type, CallId, MediaId, ets:lookup(?MODULE, {CallId, MediaId})),
	{noreply, State};

handle_cast(Cast, State) ->
	error_logger:error_msg("RADIUS notify backend: ~p - strange cast: ~p~n", [self(), Cast]),
	{stop, {error, {unknown_cast, Cast}}, State}.

handle_info(Info, State) ->
	error_logger:error_msg("RADIUS notify backend: ~p - strange info: ~p~n", [self(), Info]),
	{stop, {error, {unknown_info, Info}}, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, _State) ->
	{memory, Bytes} = erlang:process_info(self(), memory),
	error_logger:info_msg("RADIUS notify backend: ~p - terminated due to reason [~p] (allocated ~b bytes)", [self(), Reason, Bytes]).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions %%
%%%%%%%%%%%%%%%%%%%%%%%%

radius_process(start, CallId, MediaId, []) ->
	{ok, RadAcctServers} = application:get_env(rtpproxy, radacct_servers),
	Req = #rad_accreq{
		servers = RadAcctServers,
		login_time = os:timestamp(),
		std_attrs=[{?Acct_Session_Id, CallId}],
		vend_attrs = [{?Cisco, [{?h323_connect_time, date_time_fmt()}]}]
	},
	eradius_acc:acc_start(Req),
	ets:insert_new(?MODULE, {{CallId, MediaId}, {Req, {0,0,0}}});
radius_process(interim_update, CallId, MediaId, [{{CallId, MediaId}, {Req0, PrevTimestamp}}]) ->
	Timestamp = os:timestamp(),
	case to_seconds(Timestamp) - to_seconds(PrevTimestamp) > 10 of
		true ->
			Login = to_now(Req0#rad_accreq.login_time),
			SessTime = to_seconds(Timestamp) - to_seconds(Login),
			eradius_acc:acc_update(Req0#rad_accreq{session_time = SessTime}),
			ets:update_element(?MODULE, {CallId, MediaId}, {2, Timestamp});
		_ ->
			% A difference between two consequent updates was too short
			ok
	end;
radius_process(stop, CallId, MediaId, [{{CallId, MediaId}, {Req0, _}}]) ->
	Req1 = eradius_acc:set_logout_time(Req0),
	Req2 = Req1#rad_accreq{
		vend_attrs = [{?Cisco, [{?h323_disconnect_time, date_time_fmt()}]}]
	},
	eradius_acc:acc_stop(Req2),
	ets:delete(?MODULE, {CallId, MediaId});
radius_process(_, _, _, _) ->
	ok.

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
