-module(ser_sup).
-behaviour(supervisor).

-export([init/1]).

init(Args) ->
	Child = {ser,{ser,start_link,Args},permanent,2000,worker,[ser]},
	{ok,{{one_for_one,10,1}, [Child]}}.

