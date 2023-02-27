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
    , join
    , innerJoin
    , leftJoin
    , on
    , where_
    , orderBy
    ) where

import Data.Text as T
import Data.Text (Text)
import Control.Monad hiding (join)
import Control.Applicative

import qualified QueryBuilder.Internal.Query as Internal
import QueryBuilder.Column
import QueryBuilder.Condition
import QueryBuilder.QueryOrder
import QueryBuilder.JoinTable
import Prelude hiding (Left)

type Query = Internal.Query
type QueryT m = Internal.QueryT Query m Bool

runQueryT :: (Monad m) => Internal.QueryT a m b -> m a
runQueryT q = (return .snd) =<< Internal.runQueryT q
{-# INLINE runQueryT #-}

select :: (Monad m) => QueryT m
select = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.Select)
{-# INLINE select #-}

update :: (Monad m) => QueryT m
update = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.Update)
{-# INLINE update #-}

insert :: (Monad m) => QueryT m
insert = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.Insert)
{-# INLINE insert #-}

delete :: (Monad m) => QueryT m
delete = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.Delete)
{-# INLINE delete #-}

from :: (Monad m) => Text -> QueryT m
from t = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.Table t)
{-# INLINE from #-}

-- | from aliases
table :: (Monad m) => Text -> QueryT m
table = from
{-# INLINE table #-}

into :: (Monad m) => Text -> QueryT m
into = from
{-# INLINE into #-}

columns :: (Monad m) => [Column] -> QueryT m
columns c = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.Columns c)
{-# INLINE columns #-}

where_ :: (Monad m) => ConditionM -> QueryT m
where_ q = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> (Internal.Where $ runConditionM q))
{-# INLINE where_ #-}

orderBy :: (Monad m) => [Column] -> Order -> QueryT m
orderBy c o = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.OrderBy c o)
{-# INLINE orderBy #-}

joinQuery :: (Monad m) => JoinType -> Text -> (a -> a) -> ConditionM -> QueryT m
joinQuery joinType table _ q = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.Join joinType table cond)
  where
    cond = runConditionM q

join :: (Monad m) => Text -> (a -> a) -> ConditionM -> QueryT m
join = joinQuery Inner
{-# INLINE join #-}

innerJoin :: (Monad m) => Text -> (a -> a) -> ConditionM -> QueryT m
innerJoin = joinQuery Inner
{-# INLINE innerJoin #-}

leftJoin :: (Monad m) => Text -> (a -> a) -> ConditionM -> QueryT m
leftJoin = joinQuery Left
{-# INLINE leftJoin #-}

on = id
{-# INLINE on #-}

