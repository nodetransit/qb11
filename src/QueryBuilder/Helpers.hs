{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module QueryBuilder.Helpers
    ( select_
    , insert_
    , update_
    , delete_
    , from_
    , into_
    , table_
    , columns_
    , Join(..)
    ) where

import QueryBuilder.Query
import QueryBuilder.Condition

select_ = defaultQuery <> Select
insert_ = defaultQuery <> Insert
update_ = defaultQuery <> Update
delete_ = defaultQuery <> Delete

from_  t = defaultQuery <> From  t
into_  t = defaultQuery <> Into  t
table_ t = defaultQuery <> Table t

columns_ c = defaultQuery <> Columns c

