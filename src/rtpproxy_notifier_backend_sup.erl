-module(rtpproxy_notifier_backend_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I,P), {I, {I, start_link, [P]}, permanent, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(radius, Params) ->
    supervisor:start_link({local, radius_sup}, ?MODULE, [radius, Params]);
start_link(notify, Params) ->
    supervisor:start_link({local, notify_sup}, ?MODULE, [notify, Params]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([radius, Params]) ->
	{ok, { {one_for_one, 5, 10}, [?CHILD(rtpproxy_notifier_backend_radius, [Params])]} };
init([notify, Params]) ->
	{ok, { {one_for_one, 5, 10}, [?CHILD(rtpproxy_notifier_backend_notify, [Params])]} }.

