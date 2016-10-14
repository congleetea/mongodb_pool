%%%------------------------------------------------------------------------
%%% File: mongodb_pool_SUITE.erl
%%% Author: Xuancong Lee[congleetea] <lixuancong@molmc.com>
%%%
%%% Created: Thursday, October 13 2016
%%%------------------------------------------------------------------------
-module(mongodb_pool_SUITE).

-author("Xuancong Lee").

-include_lib("common_test/include/ct.hrl").

-export([
         all/0
         , init_per_suite/1
         , end_per_suite/1
         ,  init_per_testcase/2
         , end_per_testcase/2
        ]).

-export([insert_and_find/1]).

all() ->
    [
     insert_and_find
     %% , insert_and_count
     %% , insert_and_delete
     %% , sort_and_limit
    ].

init_per_suite(Config)->
    ok = application:start(bson),
    ok = application:start(crypto),
    ok = application:start(poolboy),
    ok = application:start(mongodb),
    ok = application:start(mongodb_pool),
    Config.

end_per_suite(_Config) ->
    application:stop(mongodb_pool),
    application:stop(mongodb),
    application:stop(poolboy),
    application:stop(crypto),
    application:stop(bson),
    ok.

init_per_testcase(Case, Config) ->
    [{pool_name, mongo_test_pool},{collection, cttest} | Config].

end_per_testcase(_Case, Config) ->
    ok.

insert_and_find(Config)->
    PoolName = ?config(pool_name, Config),
    Collection = ?config(collection, Config),
    P_SUP = whereis(mongodb_pool_sup),
    io:format("~n~p:~p:P_SUP=~p~n", [?MODULE, ?LINE, P_SUP]),
    %% mongodb_pool:insert(PoolName, Collection, {<<"name">>, <<"cong1">>, <<"sex">>, <<"man">>, <<"age">>, 26}),
    true.
