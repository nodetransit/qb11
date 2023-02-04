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
    -- , condition
    , QueryCondition
    -- , equals
    -- , notEquals
    -- , isNull
    -- , isNotNull
    -- , like
    -- , and
    -- , (&&)
    -- , (&&...)
    -- , (||)
    -- , (||...)
    -- , or
    -- , null
    -- , true
    -- , false
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

runConditionT :: (Monad m) => Internal.ConditionT a m b -> m b
runConditionT q = do
    (r, _) <- Internal.runConditionT q
    return r

