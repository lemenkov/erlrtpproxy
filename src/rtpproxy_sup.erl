-module(rtpproxy_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({global, ?MODULE}, ?MODULE, []).

init([]) ->
	Child = {rtpproxy,{rtpproxy,start_link,[ignored]},permanent,2000,worker,[rtpproxy]},
	{ok,{{one_for_one,10,1}, [Child]}}.

