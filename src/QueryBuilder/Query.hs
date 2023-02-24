{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module QueryBuilder.Query
    ( Internal.Query(..)
    , QueryT
    , runQueryT
    , select
    , update
    , insert
    , delete
    , from
    , table
    , into
    , columns
    -- , where_
    , whereCondition
    , orderBy
    ) where

import Data.Text as T
import Data.Text (Text)
import Control.Monad
import Control.Applicative

import qualified QueryBuilder.Internal.Query as Internal
import QueryBuilder.Column
import QueryBuilder.Condition
import QueryBuilder.QueryOrder

type Query = Internal.Query
type QueryT m = Internal.QueryT Query m Bool

runQueryT :: (Monad m) => Internal.QueryT a m b -> m a
runQueryT q = (return .snd) =<< Internal.runQueryT q

select :: (Monad m) => QueryT m
select = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.Select)

update :: (Monad m) => QueryT m
update = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.Update)

insert :: (Monad m) => QueryT m
insert = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.Insert)

delete :: (Monad m) => QueryT m
delete = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.Delete)

from :: (Monad m) => Text -> QueryT m
from t = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.Table t)

-- | from aliases
table :: (Monad m) => Text -> QueryT m
table = from
into :: (Monad m) => Text -> QueryT m
into = from

columns :: (Monad m) => [Column] -> QueryT m
columns c = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.Columns c)

-- where_ :: (Monad m) => QueryCondition -> QueryT m
-- where_ q = Internal.QueryT $ do
--     return (True, Internal.defaultQuery <> Internal.Where q)

whereCondition :: (Monad m) => ConditionM -> QueryT m
whereCondition q = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> (Internal.Where $ runConditionM q))

orderBy :: (Monad m) => [Column] -> Order -> QueryT m
orderBy c o = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.OrderBy c o)

