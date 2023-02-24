{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

{-| collection of smart constructors
 -}
module QueryBuilder.Helpers
    ( select_
    , insert_
    , update_
    , delete_
    , from_
    , into_
    , table_
    , columns_
    ) where

import QueryBuilder.Internal.Query
import QueryBuilder.JoinTable
import QueryBuilder.Condition

select_ = defaultQuery <> Select
insert_ = defaultQuery <> Insert
update_ = defaultQuery <> Update
delete_ = defaultQuery <> Delete

table_ t = defaultQuery <> Table t
from_  t = defaultQuery <> Table t
into_  t = defaultQuery <> Table t

columns_ c = defaultQuery <> Columns c

-- where_ :: (Condition, [Text]) -> Query
-- where_ predicate@(condition, bindings@(b:bs)) = 

