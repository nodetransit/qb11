{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}


module QueryBuilder.Condition
    ( ConditionT
    -- , Condition(..)
    , runConditionT
    , condition
    , QueryCondition
    , query
    , bindings
    , equals
    -- , notEquals
    -- , isNull
    -- , isNotNull
    -- , like
    , and
    -- , (&&)
    -- , (&&...)
    -- , (||)
    -- , (||...)
    -- , or
    -- , null
    -- , true
    , false
    -- , begin
    ) where

import Data.Text as T hiding (null)
import Data.Text (Text)
import Control.Monad
import Control.Monad.Identity
import Control.Applicative
import Prelude hiding (and, or, null, (&&), (||))

import           QueryBuilder.Operator
import qualified QueryBuilder.Internal.Condition as Internal

type QueryCondition = Internal.QueryCondition
type ConditionT m   = Internal.ConditionT QueryCondition m Bool
type Condition      = Internal.ConditionT QueryCondition Identity Bool

runConditionT :: (Monad m) => Internal.ConditionT a m b -> m a
runConditionT q = (return . snd) =<< Internal.runConditionT q

query    = Internal.query
bindings = Internal.bindings

condition :: (Monad m) => Text -> QueryCondition -> ConditionT m
condition left right = Internal.ConditionT $ do
    return (True, Internal.condition left right)
{-# INLINABLE condition #-}

equals :: Text -> QueryCondition
equals v = Internal.Condition "= ?" [v]

null = Internal.null
{-# INLINE null #-}

true = Internal.true
{-# INLINE true #-}

false = Internal.false
{-# INLINE false #-}

and :: (Monad m) => Text -> QueryCondition -> ConditionT m
and left right = do
    Internal.ConditionT $ return (False, Internal.and)
    condition left right
{-# INLINABLE and #-}

or :: (Monad m) => Text -> QueryCondition -> ConditionT m
or left right = do
    Internal.ConditionT $ return (False, Internal.or)
    condition left right
{-# INLINABLE or #-}

