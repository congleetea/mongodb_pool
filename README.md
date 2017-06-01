# mongodb_pool

- mongodb_pool是依赖mongodb和poolboy的一个运用。可以提前启动一定数量的mongodb数据库连接，
供客户端使用, 也可以设置一个超调值，当连接不够用的时候，适当的增加一些连接。

## mongodb_pool的配置
````````````````````
[
 {mongodb_pool, [
                 {global_or_local, local},  %% local or global
                 {pools, [             %% 可以用tuple在list里面增加不同数据库的池子。
                          {mongo_test_pool, [
                                              {size, 5},
                                              {max_overflow, 5}
                                             ], [
                                                 {host, "localhost"},
                                                 {port, 27017},
                                                 {login, <<"Username">>},    %% mognodb需要认证时加上，不要就去掉。
                                                 {password, <<"Password">>},
                                                 {database, <<"test">>}
                                                ]}
                         ]}
                ]}
].

````````````````````
## 编译测试
````````````````````
$ make
$ erl -pa ebin -pa ./deps/*/ebin
> application:ensure_all_started(mongodb_pool).
{ok,[poolboy,bson,crypto,mongodb,mongodb_pool]}
````````````````````
## Note 在emqttd中认证的问题
````````````````````
<<"Can't pass authentification">> in mc_auth_logic:scram_sha_1_auth
````````````````````
单独使用mongodb_pool的时候是没有问题的，但是在emqttd 中使用的时候就会提示这个问题。原因是relx.conf中的pbkdf2没有load.

## 数据库操作
Selector是数据库操作必须的。因为很多时候需要按条件操作。最外层是一个tuple， 里面可以通过and，or，not这些逻辑运算和mongodb提供的算数比较组合。

### 插入
````````````````````
> mongodb_pool:insert(PoolName, Collection, Docs).
````````````````````
Docs可以是tuple，map或者以它们为元素的list：

````````````````````
> mongodb_pool:insert(PoolName, Collection, {<<"x">>, 1, <<"y">>, 2}).
> mongodb_pool:insert(PoolName, Collection, [{<<"x">>, 1, <<"y">>, 2}]).
> mongodb_pool:insert(PoolName, Collection, #{<<"x">>, 1, <<"y">>, 2}).
> mongodb_pool:insert(PoolName, Collection, [#{<<"x">>, 1, <<"y">>, 2}]).
````````````````````

### 查询（结果以map形式返回）
````````````````````
> mongodb_pool:find(PoolName, Collection, Selector).
> mongodb_pool:find(PoolName, Collection, Selector, ArgsList).
````````````````````

````````````````````
> mongodb_pool:find_one(mongo_test_pool, <<"test">>, #{<<"p">> => 1}).
> mongodb_pool:find_one(mongo_test_pool, <<"test">>, {<<"p">>, 1}).
> mongodb_pool:find(mongo_test_pool, <<"test">>, #{<<"p">> => 1}).
> mongodb_pool:find(mongo_test_pool, <<"test">>, {<<"p">> , 1}).
# 只显示_id和<<"p">>
> mongodb_pool:find(mongo_test_pool, <<"test">>, {<<"p">>, 1}, #{projector => #{<<"p">> => true}}).
> mongodb_pool:find(mongo_test_pool, <<"test">>, {<<"p">>, 1}, #{projector => #{<<"p">> => true}}).
````````````````````

- 限制性查询
  mongodb_pool:find_one(PoolName, Collection, Selector)
````````````
> mongodb_pool:find_limit(mongo_test_pool, <<"test">>, #{<<"a">> => 1}, #{projector => #{<<"a">> => 1}, skip => 10, batchsize => 1}).
````````````

### 更新

Command:  {$set/$unset/$inc/..., TupleDocs}

`````````
# 只更新一条，如果不存在不会新建.
> mongodb_pool:update(mongo_test_pool, <<"test">>, {<<"a">>, 1}, #{<<"$set">> => #{<<"a">> => 2}}).
{true,#{<<"n">> => 1,<<"nModified">> => 1}}
# 最后一个参数表示是否更新多条
> mongodb_pool:update(mongo_test_pool, <<"test">>, {<<"a">>, 1}, #{<<"$set">> => #{<<"a">> => 2, <<"b">> => 1}}, false, false).
> mongodb_pool:update(mongo_test_pool, <<"test">>, {<<"a">>, 1}, #{<<"$set">> => #{<<"a">> => 2, <<"b">> => 1}}, false, true).
# 倒数第二个参数表示如果没有相关记录是否插入(同时插入<<"a">>, <<"b">>, <<"c">>)
> mongodb_pool:update(mongo_test_pool, <<"test">>, {<<"c">>, 1}, #{<<"$set">> => #{<<"a">> => 2, <<"b">> => 1}}, true, true).
`````````

### 删除
- mongodb_pool:delete(PoolName, Collection, Selector)
`````````
> mongodb_pool:delete(mongo_test_pool, <<"test">>, {<<"p">>, 1}).
{true,#{<<"n">> => 9}}
> mongodb_pool:delete_one(mongo_test_pool, <<"test">>, {<<"a">>, 1}).
{true,#{<<"n">> => 1}}
`````````
### tips
处理ISODate时间问题：
`````````
> mongodb_pool:insert(mongo_test_pool, <<"test">>, {<<"time">>, {1496,289436,401239}}).
{{true,#{<<"n">> => 1}},
 {<<"time">>,
  {1496,289436,401239},
  <<"_id">>,
  {<<89,47,144,225,54,96,68,41,149,0,0,1>>}}}
> mongodb_pool:find(mongo_test_pool, <<"test">>, {}).
[ #{<<"_id">> => {<<89,47,144,225,54,96,68,41,149,0,0,1>>},
   <<"time">> => {1496,289436,401000}}]
`````````

查询mongodb：

`````````
> db.test.find()
{ "_id" : ObjectId("592f90e13660442995000001"), "time" : ISODate("2017-06-01T03:57:16.401Z") }
`````````

