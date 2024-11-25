# qb11 #

> Query Builder

## Select Query ##

```
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (and, or, null, Left, Right)
import Data.Text as T hiding (null, length, head, tail, groupBy)
import Control.Monad.Identity hiding (join)

import QueryBuilder
import QueryBuilder.PostgreSql

buildSelectExample :: Query
buildSelectExample = (runIdentity . runQueryT) $ do
    comment "test select query\n\nexample"
    select
    limit 12
    offset 18
    distinct
    from "users"
    columns [ "users.id"
            , column_ "COUNT(id)" (as "count")
            , column_ "CONCAT(firstname, ' ', lastname)" (as "full_name")
            , "users.country"
            , "user_infos.address"
            ]
    where_ $ do
        condition "deleted" (equals false)
    orderBy [ "users.registered"
            , "user_infos.age"
            ]
            asc
    groupBy [column "users.country"]
    having $ do
        condition "count" (gte "5")
    join "user_infos" on $ do
        condition "user_infos.uid" (equalsRaw "users.id")
        and "user_infos.email" isNotNull
    leftJoin_ "transactions" (as "tx") on $ do
        condition "tx.uid" (equalsRaw "users.id")
        and "tx.failed" (equals true)
```

since it is a builder, the order doesn't really matter


## Testing ##

```
stack test --file-watch --test-arguments "--fail-fast"
```
