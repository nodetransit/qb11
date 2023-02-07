{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module QueryBuilder.Condition
    ( ConditionT
    , Condition(..)
    , runConditionT
    , condition
    , QueryCondition
    , query
    , bindings
    , equals
    , notEquals
    , isNull
    , isNotNull
    , like
    , and
    -- , (&&)
    -- , (&&...)
    -- , (||)
    -- , (||...)
    , or
    , null
    , true
    , false
    , begin
    , Predicate
    , toQueryCondition
    ) where

import Data.Text as T hiding (null)
import Data.String
import Data.Text (Text)
import Control.Monad
import Control.Monad.Identity
import Control.Applicative
import Prelude hiding (and, or, null, (&&), (||))

import           QueryBuilder.Operator
import           QueryBuilder.ToText
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
    return (True, Internal.condition left right')
  where
    right' = toQueryCondition right
{-# INLINABLE condition #-}

equals :: Text -> QueryCondition
equals v = Internal.Condition "= ?" [v]
{-# INLINABLE equals #-}

notEquals :: Text -> QueryCondition
notEquals v = Internal.Condition "<> ?" [v]
{-# INLINABLE notEquals #-}

isNull :: QueryCondition
isNull = Internal.Condition "IS NULL" []
{-# INLINABLE isNull #-}

isNotNull :: QueryCondition
isNotNull = Internal.Condition "IS NOT NULL" []
{-# INLINABLE isNotNull #-}

isNot :: Text -> QueryCondition
isNot v = Internal.Condition "IS NOT ?" [v]
{-# INLINABLE isNot #-}

like :: Text -> QueryCondition
like v = Internal.Condition "LIKE ?" [v]
{-# INLINABLE like #-}

null = Internal.null
{-# INLINE null #-}

true = Internal.true
{-# INLINE true #-}

false = Internal.false
{-# INLINE false #-}

and :: (Predicate pa, Predicate pb, Monad m) => pa -> pb -> ConditionT m
and left right = do
    Internal.ConditionT $ return (False, Internal.and)
    Internal.ConditionT $ return (False, left')
    Internal.ConditionT $ return (False, right')
    -- condition left (toQueryCondition right')
  where
    left'  = toQueryCondition left
    right' = toQueryCondition right
{-# INLINABLE and #-}

or :: (Monad m) => Text -> QueryCondition -> ConditionT m
or left right = do
    Internal.ConditionT $ return (False, Internal.or)
    condition left right
{-# INLINABLE or #-}

begin :: (Monad m) => ConditionT m -> ConditionT m
begin c = do
    let q = runConditionT c
    return (False, "(")
    return (False, q)
    return (False, ")")
    return True
{-# INLINABLE begin #-}

class Predicate t where
    toQueryCondition :: t -> QueryCondition

instance Predicate QueryCondition where
    toQueryCondition = id

instance Predicate String where
    toQueryCondition s = Internal.Condition (T.pack s) []

instance Predicate Text where
    toQueryCondition t = Internal.Condition t []

instance IsString QueryCondition where
    fromString s = Internal.Condition (T.pack s) []
