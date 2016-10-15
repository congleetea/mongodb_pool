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
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

-export([insert_and_find/1
        ,insert_and_count/1
        ,insert_and_update/1
        ,insert_and_delete/1
        ,sort_and_limit/1
        ,insert_map/1]).

all() ->
    [
     insert_and_find
    , insert_and_count
    , insert_and_update
    , insert_and_delete
    , sort_and_limit
    , insert_map
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
    [{pool_name, mongo_test_pool},{collection, atom_to_binary(Case, utf8)} | Config].

end_per_testcase(_Case, _Config) ->
    ok.

insert_and_find(Config)->
    PoolName = ?config(pool_name, Config),
    Collection = ?config(collection, Config),
    mongodb_pool:delete(PoolName, Collection, {}),
    InsertOneItem = mongodb_pool:insert(PoolName, Collection,
                                        {<<"name">>, <<"cong1">>, <<"gender">>, <<"male">>, <<"age">>, 26}),
    io:format("insert return: ~p~n", [InsertOneItem]),
    Find = mongodb_pool:find(PoolName, Collection, {<<"name">>, <<"cong1">>}),
    io:format("FIND: ~p~n", [Find]),
    true = match_bson(InsertOneItem, Find),

    InsertManyItems = mongodb_pool:insert(PoolName, Collection,
                                          [
                                           {<<"name">>, <<"ting1">>, <<"gender">>, <<"female">>, <<"age">>, 21},
                                           {<<"name">>, <<"ting2">>, <<"gender">>, <<"female">>, <<"age">>, 22},
                                           {<<"name">>, <<"ting3">>, <<"gender">>, <<"female">>, <<"age">>, 23}
                                          ]),
    io:format("~n~p:~p:InsertManyItems=~p~n", [?MODULE, ?LINE, InsertManyItems]),
    FindMany = mongodb_pool:find(PoolName, Collection, {<<"gender">>, <<"female">>}),
    io:format("~n~p:~p:FindMany=~p~n", [?MODULE, ?LINE, FindMany]),
    true = match_bson(InsertManyItems, FindMany),

    %% find_one.
    FindOne = mongodb_pool:find_one(PoolName, Collection, {<<"gender">>, <<"female">>}, [{projector, {<<"_id">>, false}}]),
    ct:log("FindOne:~p~n", [FindOne]),
    #{<<"age">> := 21,
      <<"gender">> := <<"female">>,
      <<"name">> := <<"ting1">>} = FindOne,

    %% find by projector, skip, batchsize.
    %% find by projector (<<"_id">> is found default)
    FindProjector = mongodb_pool:find(PoolName, Collection, {<<"gender">>, <<"female">>}, [{projector, {<<"_id">>, false, <<"name">>, true, <<"age">>, true}}]),
    ct:log("FindProjector=~p~n", [FindProjector]),
    lists:member(#{<<"name">> => <<"ting1">>, <<"age">> => 21}, FindProjector),
    lists:member(#{<<"name">> => <<"ting2">>, <<"age">> => 22}, FindProjector),
    lists:member(#{<<"name">> => <<"ting3">>, <<"age">> => 23}, FindProjector),

    %% find by projector and skip, skip first document.
    FindProjAndSkip = mongodb_pool:find(PoolName, Collection, {<<"gender">>, <<"female">>},
                                        [{projector, {<<"_id">>, false, <<"name">>, true, <<"age">>, true}},
                                         {skip, 1}
                                        ]),
    ct:log("FindProjAndSkip=~p~n", [FindProjAndSkip]),
    false =  lists:member(#{<<"name">> => <<"ting1">>, <<"age">> => 21}, FindProjAndSkip),
    true  = lists:member(#{<<"name">> => <<"ting2">>, <<"age">> => 22}, FindProjAndSkip),
    true  = lists:member(#{<<"name">> => <<"ting3">>, <<"age">> => 23}, FindProjAndSkip),

    %% Find with order: just show one document.
    FindPrjSkipBs1 = mongodb_pool:find(PoolName, Collection, {<<"gender">>, <<"female">>},
                                  [{projector, {<<"_id">>, false, <<"name">>, true, <<"age">>, true}},
                                   {skip, 1},
                                   {batchsize, 1}
                                  ]),
    ct:log("FindPrjSkipBs1=~p~n", [FindPrjSkipBs1]),
    false = lists:member(#{<<"name">> => <<"ting1">>, <<"age">> => 21}, FindPrjSkipBs1),
    true  = lists:member(#{<<"name">> => <<"ting2">>, <<"age">> => 22}, FindPrjSkipBs1),
    false = lists:member(#{<<"name">> => <<"ting3">>, <<"age">> => 23}, FindPrjSkipBs1),
    FindPrjSkipBs2 = mongodb_pool:find(PoolName, Collection, {<<"gender">>, <<"female">>},
                                      [{projector, {<<"_id">>, false, <<"name">>, true, <<"age">>, true}},
                                       {skip, 1},
                                       {batchsize, 2}
                                      ]),
    ct:log("FindPrjSkipBs=~p~n", [FindPrjSkipBs2]),
    false = lists:member(#{<<"name">> => <<"ting1">>, <<"age">> => 21}, FindPrjSkipBs2),
    true  = lists:member(#{<<"name">> => <<"ting2">>, <<"age">> => 22}, FindPrjSkipBs2),
    true  = lists:member(#{<<"name">> => <<"ting3">>, <<"age">> => 23}, FindPrjSkipBs2).

insert_and_count(Config) ->
    PoolName = ?config(pool_name, Config),
    Collection = ?config(collection, Config),
    mongodb_pool:delete(PoolName, Collection, {}),
    InsertManyItems = mongodb_pool:insert(PoolName, Collection,
                                          [
                                           {<<"name">>, <<"ting">>, <<"sex">>, <<"woman">>, <<"age">>, 21},
                                           {<<"name">>, <<"ting">>, <<"sex">>, <<"woman">>, <<"age">>, 22},
                                           {<<"name">>, <<"ting">>, <<"sex">>, <<"woman">>, <<"age">>, 23}
                                          ]),
    io:format("~n~p:~p:InsertManyItems=~p~n", [?MODULE, ?LINE, InsertManyItems]),
    3 = mongodb_pool:count(PoolName, Collection, {<<"name">>, <<"ting">>}).

insert_and_update(Config) ->
    PoolName = ?config(pool_name, Config),
    Collection = ?config(collection, Config),
    mongodb_pool:delete(PoolName, Collection, {}),
    mongodb_pool:insert(PoolName, Collection,
                        {<<"_id">>, 100,
                         <<"sku">>, <<"abc123">>,
                         <<"quantity">>, 250,
                         <<"instock">>, true,
                         <<"reorder">>, false,
                         <<"details">>, {<<"model">>, "14Q2", <<"make">>, "xyz"},
                         <<"tags">>, ["apparel", "clothing"],
                         <<"ratings">>, [{<<"by">>, "ijk", <<"rating">>, 4}]}
                       ),
    %% check data inserted
    [Res] = mongodb_pool:find(PoolName, Collection, {<<"_id">>, 100}),
    ct:log("Res: ~p~n", [Res]),
    #{<<"_id">> := 100,
      <<"sku">> := <<"abc123">>,
      <<"quantity">> := 250,
      <<"instock">> := true,
      <<"reorder">> := false,
      <<"details">> := #{<<"model">> := "14Q2", <<"make">> := "xyz"},
      <<"tags">> := ["apparel", "clothing"],
      <<"ratings">> := [#{<<"by">> := "ijk", <<"rating">> := 4}]} = Res,

    %% update existent fields
    Command = #{
      <<"quantity">> => 500,
      <<"details">> => #{<<"model">> => "14Q3"},  %with flatten_map there is no need to specify non-changeble data
      <<"tags">> => ["coats", "outerwear", "clothing"]
     },
    mongodb_pool:update(PoolName, Collection, {<<"_id">>, 100}, #{<<"$set">> => bson:flatten_map(Command)}),

    %% check data updated
    [Res1] = mongodb_pool:find(PoolName, Collection, {<<"_id">>, 100}),
    #{<<"_id">> := 100,
      <<"sku">> := <<"abc123">>,
      <<"quantity">> := 500,
      <<"instock">> := true,
      <<"reorder">> := false,
      <<"details">> := #{<<"model">> := "14Q3", <<"make">> := "xyz"},
      <<"tags">> := ["coats", "outerwear", "clothing"],
      <<"ratings">> := [#{<<"by">> := "ijk", <<"rating">> := 4}]} = Res1,

    %% update non existent fields
    Command1 = {<<"$set">>, {<<"expired">>, true}},
    mongodb_pool:update(PoolName, Collection, {<<"_id">>, 100}, Command1),

    %% check data updated
    [Res2] = mongodb_pool:find(PoolName, Collection, {<<"_id">>, 100}),
    ct:log("Got ~p", [Res2]),
    #{<<"_id">> := 100,
      <<"sku">> := <<"abc123">>,
      <<"quantity">> := 500,
      <<"instock">> := true,
      <<"reorder">> := false,
      <<"details">> := #{<<"model">> := "14Q3", <<"make">> := "xyz"},
      <<"tags">> := ["coats", "outerwear", "clothing"],
      <<"ratings">> := [#{<<"by">> := "ijk", <<"rating">> := 4}],
      <<"expired">> := true} = Res2,

    %% update embedded fields
    Command2 = {<<"$set">>, {<<"details.make">>, "zzz"}},
    mongodb_pool:update(PoolName, Collection, {<<"_id">>, 100}, Command2),

                                                %check data updated
    [Res3] = mongodb_pool:find(PoolName, Collection, {<<"_id">>, 100}),
    ct:log("Got ~p", [Res3]),
    #{<<"_id">> := 100,
      <<"sku">> := <<"abc123">>,
      <<"quantity">> := 500,
      <<"instock">> := true,
      <<"reorder">> := false,
      <<"details">> := #{<<"model">> := "14Q3", <<"make">> := "zzz"},
      <<"tags">> := ["coats", "outerwear", "clothing"],
      <<"ratings">> := [#{<<"by">> := "ijk", <<"rating">> := 4}],
      <<"expired">> := true} = Res3,

    %% update list elements
    Command3 = {<<"$set">>, {
                    <<"tags.1">>, "rain gear",
                    <<"ratings.0.rating">>, 2
                   }},
    mongodb_pool:update(PoolName, Collection, {<<"_id">>, 100}, Command3),
    [Res4] = mongodb_pool:find(PoolName, Collection, {<<"_id">>, 100}),
    ct:log("Got ~p", [Res4]),
    #{<<"_id">> := 100,
      <<"sku">> := <<"abc123">>,
      <<"quantity">> := 500,
      <<"instock">> := true,
      <<"reorder">> := false,
      <<"details">> := #{<<"model">> := "14Q3", <<"make">> :="zzz"},
      <<"tags">> := ["coats", "rain gear", "clothing"],
      <<"ratings">> := [#{<<"by">> := "ijk", <<"rating">> := 2}],
      <<"expired">> := true} = Res4.

insert_and_delete(Config) ->
    PoolName = ?config(pool_name, Config),
    Collection = ?config(collection, Config),
    mongodb_pool:delete(PoolName, Collection, {}),
    InsertOneItem = mongodb_pool:insert(PoolName, Collection,
                                        {<<"name">>, <<"cong">>, <<"sex">>, <<"man">>, <<"age">>, 26}),
    io:format("insert return: ~p~n", [InsertOneItem]),
    Find = mongodb_pool:find(PoolName, Collection, {<<"name">>, <<"cong">>}),
    io:format("FIND: ~p~n", [Find]),
    true = match_bson(InsertOneItem, Find),

    mongodb_pool:insert(PoolName, Collection,
                        [
                         {<<"name">>, <<"ting">>, <<"sex">>, <<"woman">>, <<"age">>, 21},
                         {<<"name">>, <<"ting">>, <<"sex">>, <<"woman">>, <<"age">>, 22},
                         {<<"name">>, <<"ting">>, <<"sex">>, <<"woman">>, <<"age">>, 23}
                        ]),
    mongodb_pool:delete(PoolName, Collection, {'$or', [{<<"name">>, <<"cong">>},{<<"name">>, <<"ting">>}]}).

sort_and_limit(Config) ->
    PoolName = ?config(pool_name, Config),
    Collection = ?config(collection, Config),
    mongodb_pool:delete(PoolName, Collection, {}),
    %% insert test data
    mongodb_pool:insert(PoolName, Collection, [
                                          {<<"key">>, <<"test">>, <<"value">>, <<"two">>, <<"tag">>, 2},
                                          {<<"key">>, <<"test">>, <<"value">>, <<"one">>, <<"tag">>, 1},
                                          {<<"key">>, <<"test">>, <<"value">>, <<"four">>, <<"tag">>, 4},
                                          {<<"key">>, <<"another">>, <<"value">>, <<"five">>, <<"tag">>, 5},
                                          {<<"key">>, <<"test">>, <<"value">>, <<"three">>, <<"tag">>, 3}
                                         ]),

    %% test match and sort
    %% db.<Collection>.aggregate({$match: {"key": "test"}}, {$sort: {"tag": 1}})
    %% mongodb_pool:find_sort(PoolName, Collection, Selector1, Sort1),
    PassSeq = mongodb_pool:aggregate(PoolName, Collection, {<<"key">>, <<"test">>}, [{sort, {<<"tag">>, 1}}]),
    [
     #{<<"key">> := <<"test">>, <<"value">> := <<"one">>, <<"tag">> := 1},
     #{<<"key">> := <<"test">>, <<"value">> := <<"two">>, <<"tag">> := 2},
     #{<<"key">> := <<"test">>, <<"value">> := <<"three">>, <<"tag">> := 3},
     #{<<"key">> := <<"test">>, <<"value">> := <<"four">>, <<"tag">> := 4}
    ] = PassSeq,
    InvertedSeq = mongodb_pool:aggregate(PoolName, Collection, [{match, {<<"key">>, <<"test">>}}, {sort, {<<"tag">>, -1}}]),
    [
     #{<<"key">> := <<"test">>, <<"value">> := <<"four">>, <<"tag">> := 4},
     #{<<"key">> := <<"test">>, <<"value">> := <<"three">>, <<"tag">> := 3},
     #{<<"key">> := <<"test">>, <<"value">> := <<"two">>, <<"tag">> := 2},
     #{<<"key">> := <<"test">>, <<"value">> := <<"one">>, <<"tag">> := 1}
    ] = InvertedSeq,

    %% test match & sort with limit
    %% db.sort_and_limit.aggregate({$match: {"key": "test"}}, {$sort: {"tag": -1}}, {$limit: 1})
    SortLimit = mongodb_pool:aggregate(PoolName, Collection,
                                       [{match, {{<<"key">>, <<"test">>}}},
                                        {sort, {<<"tag">>, 1}},
                                        {limit, 1}]),
    [#{<<"key">> := <<"test">>, <<"value">>:= <<"one">>, <<"tag">> := 1}] = SortLimit,

    %% test match & sort with limit
    %% db.sort_and_limit.aggregate({$match: {"key": "test"}}, {$sort: {"tag": 1}}, {$limit: 1})
    ProjSortLimit = mongodb_pool:aggregate(PoolName, Collection,
                                       [{match, {{<<"key">>, <<"test">>}}},
                                        {project, [{<<"_id">>, 0, <<"key">>, 1, <<"value">>, 1}]},
                                        {sort, {<<"tag">>, 1}},
                                        {limit, 1}]),
    [#{<<"key">> := <<"test">>, <<"value">>:= <<"one">>}] = ProjSortLimit,
    Config.

insert_map(Config) ->
    PoolName = ?config(pool_name, Config),
    Collection = ?config(collection, Config),
    mongodb_pool:delete(PoolName, Collection, {}),
    Map = #{<<"name">> => <<"Yankees">>,
            <<"home">> => #{<<"city">> => <<"New York">>, <<"state">> => <<"NY">>},
            <<"league">> => <<"American">>},
    mongodb_pool:insert(PoolName, Collection, Map),
    [Res1] = mongodb_pool:find(PoolName, Collection, {<<"home">>, {<<"city">>, <<"New York">>, <<"state">>, <<"NY">>}}, [{projector, {<<"_id">>, false}}]),
    #{<<"home">> := #{<<"city">> := <<"New York">>, <<"state">> := <<"NY">>},
      <<"league">> := <<"American">>, <<"name">> := <<"Yankees">>} = Res1,

    Maps = [#{<<"name">> => <<"Yankees">>,
              <<"home">> => #{<<"city">> => <<"New York">>, <<"state">> => <<"NY">>},
              <<"league">> => <<"American">>},
            #{<<"name">> => <<"Xclee">>,
              <<"home">> => #{<<"city">> => <<"Zhaotong">>, <<"state">> => <<"YN">>},
              <<"league">> => <<"China">>},
            #{<<"name">> => <<"LeeXc">>,
              <<"home">> => #{<<"city">> => <<"Shenzhen">>, <<"state">> => <<"GD">>},
              <<"league">> => <<"China">>}
           ],
    mongodb_pool:insert(PoolName, Collection, Maps),
    Res2 = mongodb_pool:find(PoolName, Collection, {<<"league">>, <<"China">>}, [{projector, {<<"_id">>, false}}]),
    ct:log("Res2:~p~n", [Res2]),
    true = lists:member(#{<<"name">> => <<"Xclee">>,
                          <<"home">> => #{<<"city">> => <<"Zhaotong">>, <<"state">> => <<"YN">>},
                          <<"league">> => <<"China">>}, Res2),
    true = lists:member(#{<<"name">> => <<"LeeXc">>,
                          <<"home">> => #{<<"city">> => <<"Shenzhen">>, <<"state">> => <<"GD">>},
                          <<"league">> => <<"China">>}, Res2),
    ok.

%% @private
match_bson(Tuple1, Tuple2) when length(Tuple1) /= length(Tuple2) -> false;
match_bson(Tuple1, Tuple2) ->
    try
        lists:foldr(
          fun(Elem, Num) ->
                  Elem2 = lists:nth(Num, Tuple2),
                  Sorted = lists:sort(bson:fields(Elem)),
                  Sorted = lists:sort(bson:fields(Elem2))
          end, 1, Tuple1)
    catch
        _:_ -> false
    end,
    true.
