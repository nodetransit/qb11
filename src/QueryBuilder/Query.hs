{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module QueryBuilder.Query
    ( Internal.Query(..)
    , runQuery
    , QueryT
    , runQueryT
    , select
    , update
    , insert
    , delete
    , distinct
    , from
    , from_
    , table
    , into
    , columns
    , column
    , column_
    , set
    , values
    , join
    , join_
    , innerJoin
    , innerJoin_
    , outerJoin
    , outerJoin_
    , leftJoin
    , leftJoin_
    , rightJoin
    , rightJoin_
    , crossJoin
    , crossJoin_
    , as
    , on
    , where_
    , groupBy
    , having
    , orderBy
    , desc
    , asc
    , limit
    , comment
    , comments
    ) where

import Data.Text as T hiding (groupBy)
import Data.Text (Text)
import Data.Semigroup
import Control.Monad hiding (join)
import Control.Monad.Identity hiding (join)
import Control.Applicative

import qualified QueryBuilder.Internal.Query as Internal
import QueryBuilder.Alias as Alias
import QueryBuilder.Column
import QueryBuilder.Condition
import QueryBuilder.QueryOrder
import QueryBuilder.QueryOrder as Order
import QueryBuilder.JoinTable
import Prelude hiding (Left, Right)

type Query = Internal.Query
type QueryT m = Internal.QueryT Query m Bool

runQueryT :: (Monad m) => Internal.QueryT a m b -> m a
runQueryT q = (return .snd) =<< Internal.runQueryT q
{-# INLINE runQueryT #-}

runQuery = runIdentity . runQueryT
{-# INLINE runQuery #-}

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

distinct :: (Monad m) => QueryT m
distinct = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.Distinct)
{-# INLINE distinct #-}

from :: (Monad m) => Text -> QueryT m
from t = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.Table t)
{-# INLINE from #-}

from_ :: (Monad m) => Text -> Alias -> QueryT m
from_ t a = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.TableAlias t a)
{-# INLINE from_ #-}

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

column = Column
{-# INLINE column #-}

column_ = ColumnAlias
{-# INLINE column_ #-}

values :: (Monad m) => [[Text]] -> QueryT m
values v = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.Values v)
{-# INLINE values #-}

set :: (Monad m) => [(Text, Text)] -> QueryT m
set v = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.Set v)
{-# INLINE set #-}

where_ :: (Monad m) => ConditionM -> QueryT m
where_ q = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> (Internal.Where $ runConditionM q))
{-# INLINE where_ #-}

orderBy :: (Monad m) => [Column] -> Order -> QueryT m
orderBy c o = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.OrderBy c o)
{-# INLINE orderBy #-}

desc :: Order
desc = Order.Desc
{-# INLINE desc #-}

asc :: Order
asc = Order.Asc
{-# INLINE asc #-}

joinQuery :: (Monad m) => JoinType -> Alias -> Text -> (a -> a) -> ConditionM -> QueryT m
joinQuery joinType alias table _ q = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.JoinAlias joinType table alias cond)
  where
    cond = runConditionM q

join :: (Monad m) => Text -> (a -> a) -> ConditionM -> QueryT m
join = joinQuery Inner Alias.None
{-# INLINE join #-}

innerJoin :: (Monad m) => Text -> (a -> a) -> ConditionM -> QueryT m
innerJoin = joinQuery Inner Alias.None
{-# INLINE innerJoin #-}

leftJoin :: (Monad m) => Text -> (a -> a) -> ConditionM -> QueryT m
leftJoin = joinQuery Left Alias.None
{-# INLINE leftJoin #-}

rightJoin :: (Monad m) => Text -> (a -> a) -> ConditionM -> QueryT m
rightJoin = joinQuery Right Alias.None
{-# INLINE rightJoin #-}

outerJoin :: (Monad m) => Text -> (a -> a) -> ConditionM -> QueryT m
outerJoin = joinQuery Outer Alias.None
{-# INLINE outerJoin #-}

crossJoin :: (Monad m) => Text -> (a -> a) -> ConditionM -> QueryT m
crossJoin = joinQuery Cross Alias.None
{-# INLINE crossJoin #-}

join_ :: (Monad m) => Text -> Alias -> (a -> a) -> ConditionM -> QueryT m
join_ table alias = joinQuery Inner alias table
{-# INLINE join_ #-}

innerJoin_ :: (Monad m) => Text -> Alias -> (a -> a) -> ConditionM -> QueryT m
innerJoin_ table alias = joinQuery Inner alias table
{-# INLINE innerJoin_ #-}

leftJoin_ :: (Monad m) => Text -> Alias -> (a -> a) -> ConditionM -> QueryT m
leftJoin_ table alias = joinQuery Left alias table
{-# INLINE leftJoin_ #-}

rightJoin_ :: (Monad m) => Text -> Alias -> (a -> a) -> ConditionM -> QueryT m
rightJoin_ table alias = joinQuery Right alias table
{-# INLINE rightJoin_ #-}

outerJoin_ :: (Monad m) => Text -> Alias -> (a -> a) -> ConditionM -> QueryT m
outerJoin_ table alias = joinQuery Outer alias table
{-# INLINE outerJoin_ #-}

crossJoin_ :: (Monad m) => Text -> Alias -> (a -> a) -> ConditionM -> QueryT m
crossJoin_ table alias = joinQuery Cross alias table
{-# INLINE crossJoin_ #-}

as :: Text -> Alias
as t = As t

on = id
{-# INLINE on #-}

groupBy :: (Monad m) => [Column] -> QueryT m
groupBy c = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.GroupBy c)
{-# INLINE groupBy #-}

having :: (Monad m) => ConditionM -> QueryT m
having q = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> (Internal.Having $ runConditionM q))
{-# INLINE having #-}

limit :: (Monad m) => Int -> QueryT m
limit n = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.Limit n)
{-# INLINE limit #-}

comments :: (Monad m) => [Text] -> QueryT m
comments c = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.Comment c)
{-# INLINE comments #-}

comment :: (Monad m) => Text -> QueryT m
comment c = comments [c]
{-# INLINE comment #-}

