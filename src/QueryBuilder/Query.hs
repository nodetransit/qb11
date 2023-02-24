{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module QueryBuilder.Query
    ( Internal.Query(..)
    , QueryT
    , runQueryT
    , select
    , from
    , table
    , into
    , columns
    ) where

import Data.Text as T
import Data.Text (Text)
import Control.Monad
import Control.Applicative

import qualified QueryBuilder.Internal.Query as Internal
import QueryBuilder.Column

type Query = Internal.Query
type QueryT m = Internal.QueryT Query m Bool

runQueryT :: (Monad m) => Internal.QueryT a m b -> m a
runQueryT q = (return .snd) =<< Internal.runQueryT q

select :: (Monad m) => QueryT m
select = Internal.QueryT $ do
    return (True, Internal.defaultQuery <> Internal.Select)

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
