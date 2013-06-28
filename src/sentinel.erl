-module(sentinel).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/0]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
	process_flag(trap_exit, true),
	{ok, TRef} = timer:send_interval(10000, check),
	error_logger:warning_msg("sentinel: started at ~p.~n", [node()]),
	{ok, TRef}.

handle_call(Call, _From, State) ->
	error_logger:error_msg("sentinel: unmatched call [~p]", [Call]),
	{stop, {error, {unknown_call, Call}}, State}.

handle_cast(Cast, State) ->
	error_logger:error_msg("sentinel: unmatched cast [~p]", [Cast]),
	{stop, {error, {unknown_cast, Cast}}, State}.

handle_info(check, State) ->
	case whereis(error_logger) of
		undefined -> ok;
		Pid ->
			{memory, MemP} = erlang:process_info(Pid, memory),
			{message_queue_len, MQL} = erlang:process_info(Pid, message_queue_len),
			MemT = erlang:memory(processes_used),
			MemP < MemT*0.5 orelse begin MQL == 0 andalso begin erlang:garbage_collect(Pid), error_logger:warning_msg("sentinel: memory threshold reached: ~b from ~b", [MemP, MemT]) end end,
			ok
	end,
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(Reason, TRef) ->
	{memory, Bytes} = erlang:process_info(self(), memory),
	timer:cancel(TRef),
	error_logger:warning_msg("sentinel: terminated due to reason [~p] (allocated ~b bytes)", [Reason, Bytes]).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
