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
## 数据库操作
Selector是数据库操作必须的。因为很多时候需要按条件操作。最外层是一个tuple， 里面可以通过and，or，not这些逻辑运算和mongodb提供的算数比较组合。

### 插入
mongodb_pool:insert(PoolName, Collection, Docs).

- 插入单条document

  mongodb_pool:insert(PoolName, Collection, {<<"x">>", 1, <<"y">>, 2})
  
- 插入多条documents

  mongodb_pool:insert(PoolName, Collection, [{<<"age", 11, <<"gender">>, <<"male">>}, {<<"age", 12, <<"gender">>, <<"male">>}])

- 插入map/maps
  
  和前面插入tuple一样，可以插入一条map，也可以把多条map放在list里面插入多条map。
  
### 查询（结果以map形式返回）
mongodb_pool:find(PoolName, Collection, Selector).
mongodb_pool:find(PoolName, Collection, Selector, ArgsList).

- 获取满足条件的第一条记录

  mongodb_pool:find_one(PoolName, Collection, Selector)

- 带参数查询
````````````
[
  {projector, {<<"name">>, 1/true, <<"_id">>, 0/false}}, %% 默认_id是一起返回的，下面则不返回_id，只返回name字段
  {skip, 10},        %% 跳过满足条件的前10条记录
  {batchsize, 5}]    %% 只返回满足条件的5条
````````````
````````````
  mongodb_pool:find(PoolName, Collection, Selector, 
                    [{projector, {<<"_id">>, 0, <<"name">>, 1}}，
  	                 {skip, 10},
                     {batchsize, 5}])
````````````
### 更新
mongodb_pool:update(PoolName, Collection, Selector, Command)
mongodb_pool:update(PoolName, Collection, Selector, Command, ArgsList)

Command:  {$set/$unset/$inc/..., TupleDocs}

- 更新参数

`````````
[{upsert, true/false}]  %% 如果这个document没有就新建/忽略这个记录。
`````````

### 删除
- mongodb_pool:delete(PoolName, Collection, Selector)

### 管道
mongodb_pool:aggregate(PoolName, Collection, Command)

Command 是一个list，可以包括很多特定元组，这些参数这可以实现前面的一些功能，比如skip，project，limit的功能。