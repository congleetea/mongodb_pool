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
         find_one/3, find_one/4,
         findlimit/5,
         insert/3,
         delete/3, delete_one/3,
         update/4, update/5,
         count/3, count/4,
         aggregate/4,
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
                  {ok, binary() | [binary()]}| {error, Reason::binary()}.
find(PoolName, Collection, Selector)->
    find(PoolName, Collection, Selector, []).

-spec find(PoolName::atom(), Collection::collection(),Selector::selector(), Args::proplists:proplist())->
                  {ok, binary() | [binary()]}| {error, Reason::binary()}.
find(PoolName, Collection, Selector, Args)->
    poolboy:transaction(PoolName, fun(Worker) ->
                                          Cursor = mongo:find(Worker, Collection, Selector, Args),
                                          Result = mc_cursor:rest(Cursor),
                                          mc_cursor:close(Cursor),
                                          Result
                                  end).

-spec find_one(PoolName::atom(), Collection::collection(), Selector::selector())->
                      {ok, binary() | [binary()]} | {error, Reason::binary()}.
find_one(PoolName, Collection, Selector)->
    find_one(PoolName, Collection, Selector, []).

-spec find_one(PoolName::atom(), Collection::collection(), Selector::selector(), proplists:proplist())->
                      {ok, binary() | [binary()]} | {error, Reason::binary()}.
find_one(PoolName, Collection, Selector, Args)->
    poolboy:transaction(PoolName, fun(Worker) ->
                                          mongo:find_one(Worker, Collection, Selector, Args)
                                  end).
-spec findlimit(PoolName::atom(),
                     Collection::collection(),
                     Selector::selector(),
                     Args::proplists:proplist(),
                     Limit::integer())->
                            {ok, binary() | [binary()]}| {error, Reason::binary()}.
findlimit(PoolName, Collection, Selector, Args, Limit)->
    poolboy:transaction(PoolName, fun(Worker) ->
                                          Cursor = mongo:find(Worker, Collection, Selector, Args),
                                          Result = mc_cursor:take(Cursor, Limit),
                                          mc_cursor:close(Cursor),
                                          Result
                                  end).

-spec count(PoolName::atom(), Collection::collection(), Selector::selector())->integer().
count(PoolName, Collection, Selector)->
    count(PoolName, Collection, Selector, 0).

-spec count(PoolName::atom(), Collection::collection(), Selector::selector(), Limit::integer())->integer().
count(PoolName, Collection, Selector, Args)->
    poolboy:transaction(PoolName, fun(Worker) ->
                                          mongo:count(Worker, Collection, Selector, Args)
                                  end).

-spec insert(PoolName::atom(), Collection::collection(),Docs::document())-> Docs::document().
insert(PoolName, Collection, Docs)->
    poolboy:transaction(PoolName, fun(Worker) ->
                                          mongo:insert(Worker, Collection, Docs)
                                  end).

-spec update(PoolName::atom(), Collection::collection(), Selector::selector(),Command::bson:document())->ok.
update(PoolName, Collection, Selector, Command)->
    update(PoolName, Collection, Selector, Command, []).

-spec update(PoolName::atom(), Collection::collection(), Selector::selector(),Command::bson:document(),
             Args::proplists:proplist())->ok.
update(PoolName, Collection, Selector, Command, Args)->
    poolboy:transaction(PoolName, fun(Worker)->
                                          mongo:update(Worker, Collection, Selector, Command, Args)
                                  end).

-spec delete(atom(), collection(), selector())->ok.
delete(PoolName, Collection, Selector)->
    poolboy:transaction(PoolName, fun(Worker)->
                                          mongo:delete(Worker, Collection, Selector)
                                  end).

-spec delete_one(atom(), collection(), selector())->ok.
delete_one(PoolName, Collection, Selector)->
    poolboy:transaction(PoolName, fun(Worker)->
                                          mongo:delete_one(Worker, Collection, Selector)
                                  end).

-spec aggregate(atom(), collection(), binary(), list()) -> list().
aggregate(PoolName, Collection, Q, AggArgs) ->
    poolboy:transaction(PoolName, fun(Worker)->
                                          {true, #{<<"result">> := Res}} =
                                              mongo:command(Worker,
                                                            {<<"aggregate">>, Collection, Q,AggArgs}),
                                          Res
                                  end).

-spec ensure_index(PoolName::atom(), Collection::collection(), Index::bson:document())->ok.
ensure_index(PoolName, Collection, Index)->
    poolboy:transaction(PoolName, fun(Worker)->
                                          mongo:ensure_index(Worker, Collection, Index)
                                  end).
description()->
    Description = application:get_env(?MODULE, description),
    io:format("~p~n", [Description]).
