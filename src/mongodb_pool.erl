%%%------------------------------------------------------------------
%%% File: mongodb_pool.erl
%%% Author: Xuancong Lee[congleetea] <congleetea@gmail.com>
%%%
%%% Created: Saturday, December 26 2015
%%%------------------------------------------------------------------
-module(mongodb_pool).

-author("Xuancong Lee").

%% Include
-include("../include/mongodb_pool.hrl").

%% API
-export([start/0, stop/0]).
-export([find/3, find/4,
         find_limit/4,
         find_one/3, find_one/4,
         insert/3,
         delete/3, delete_one/3,
         update/4, update/6,
         count/3, count/4,
         ensure_index/3]).
-export([description/0]).

%% ===================================================================
%% API functions
%% ===================================================================
start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

%% ===================================================================
%% Executes the given command in the specified connection.
%% ===================================================================
-spec find(PoolName::atom(), Collection::collection(),Selector::selector())->
                  [bson:document()] | error.
find(PoolName, Collection, Selector)->
    find(PoolName, Collection, Selector, #{}).

-spec find(PoolName::atom(), Collection::collection(),Selector::selector(), Projector::map())->
                  [bson:document()] | error.
%% Projector = #{projector => P}
find(PoolName, Collection, Selector, Projector)->
    poolboy:transaction(PoolName, fun(Worker) ->
                                          case mc_worker_api:find(Worker, Collection, Selector, Projector) of
                                              {ok, Cursor} ->
                                                  mc_cursor:rest(Cursor);
                                              [] ->
                                                  []
                                          end
                                  end).

-spec find_limit(PoolName::atom(), Collection::collection(),Selector::selector(), Limit::map())->
                  [bson:document()] | error.
%% Limit = #{projector => #{}, skip => NumSkip, batchsize => NumBatchSize}
find_limit(PoolName, Collection, Selector, Limit)->
    poolboy:transaction(PoolName, fun(Worker) ->
                                          case  mc_worker_api:find(Worker, Collection, Selector, Limit) of
                                              {ok, Cursor} ->
                                                  mc_cursor:next_batch(Cursor);
                                              [] ->
                                                  []
                                          end
                                  end).

-spec find_one(PoolName::atom(), Collection::collection(), Selector::selector())->
                      map()|undefined.
find_one(PoolName, Collection, Selector)->
    find_one(PoolName, Collection, Selector, #{}).

-spec find_one(PoolName::atom(), Collection::collection(), Selector::selector(), Args::map())->
                      map()|undefined.
%% Args = #{projector => #{<<"field1">> => true}, skip => Num}
find_one(PoolName, Collection, Selector, Args)->
    poolboy:transaction(PoolName, fun(Worker) ->
                                          mc_worker_api:find_one(Worker, Collection, Selector, Args)
                                  end).

-spec count(PoolName::atom(), Collection::collection(), Selector::selector())->integer().
count(PoolName, Collection, Selector)->
    count(PoolName, Collection, Selector, #{}).

-spec count(PoolName::atom(), Collection::collection(), Selector::selector(), Args::map())->integer().
%% Args = #{limit := Limit} Limit == 0 means no max, stops count if reach Limit.
count(PoolName, Collection, Selector, Args)->
    poolboy:transaction(PoolName, fun(Worker) ->
                                          mc_worker_api:count(Worker, Collection, Selector, Args)
                                  end).

-spec insert(PoolName::atom(), Collection::collection(),Docs::document())-> Docs::document().
insert(PoolName, Collection, Docs)->
    poolboy:transaction(PoolName, fun(Worker) ->
                                          mc_worker_api:insert(Worker, Collection, Docs)
                                  end).

-spec update(PoolName::atom(), Collection::collection(), Selector::selector(), map())->
                    {boolean(), map()}.
update(PoolName, Collection, Selector, Command)->
    update(PoolName, Collection, Selector, Command, false, false).

-spec update(PoolName::atom(), Collection::collection(), Selector::selector(),Command::map(), boolean(), boolean())->
                    {boolean(), map()}.
update(PoolName, Collection, Selector, Command, Upsert, MultiUpdate)->
    poolboy:transaction(PoolName, fun(Worker)->
                                          mc_worker_api:update(Worker, Collection, Selector, Command, Upsert, MultiUpdate)
                                  end).

-spec delete(atom(), collection(), selector())->{boolean(), map()}.
%% {true,#{<<"n">> => 9}}
delete(PoolName, Collection, Selector)->
    poolboy:transaction(PoolName, fun(Worker)->
                                          mc_worker_api:delete(Worker, Collection, Selector)
                                  end).

-spec delete_one(atom(), collection(), selector())->ok.
delete_one(PoolName, Collection, Selector)->
    poolboy:transaction(PoolName, fun(Worker)->
                                          mc_worker_api:delete_one(Worker, Collection, Selector)
                                  end).

-spec ensure_index(PoolName::atom(), Collection::collection(), Index::bson:document())->ok | {error, any()}.
ensure_index(PoolName, Collection, Index)->
    poolboy:transaction(PoolName, fun(Worker)->
                                          mc_worker_api:ensure_index(Worker, Collection, Index)
                                  end).
description()->
    Description = application:get_env(?MODULE, description),
    io:format("~p~n", [Description]).
