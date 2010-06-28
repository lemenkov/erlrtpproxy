-module(ser_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Child = {ser,{ser,start_link,[ignored]},permanent,2000,worker,[ser]},
	{ok,{{one_for_one,10,1}, [Child]}}.

