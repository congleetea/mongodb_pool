-module(mongodb_pool_sup).

-author("Xuancong Lee").

-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

-include("../include/mongodb_pool.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    {ok, Pools} = application:get_env(mongodb_pool, pools),
    {ok, GlobalOrLocal} = application:get_env(mongodb_pool, global_or_local),
    start_link(Pools, GlobalOrLocal).

start_link(Pools, GlobalOrLocal) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Pools, GlobalOrLocal]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([Pools, GlobalOrLocal]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
                                  PoolArgs = [{name, {GlobalOrLocal, Name}},
                                              {worker_module,mc_worker}] ++ SizeArgs,
                                  poolboy:child_spec(Name, PoolArgs, WorkerArgs)
                          end, Pools),
    {ok, {SupFlags, PoolSpecs}}.
