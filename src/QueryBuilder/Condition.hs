{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}


module QueryBuilder.Condition
    ( ConditionT
    , ConditionM(..)
    , runConditionT
    , runConditionM
    , condition
    , QueryCondition
    , rawQueryCondition
    , rawQueryConditionT
    , raw
    , clause
    , bindings
    , equals
    , equalsRaw
    , notEquals
    , notEqualsRaw
    , eq
    , eqRaw
    , neq
    , neqRaw
    , gt
    , gtRaw
    , gte
    , gteRaw
    , lt
    , ltRaw
    , lte
    , lteRaw
    , is
    , isRaw
    , isNot
    , isNotRaw
    , not
    , notRaw
    , isIn
    , isInRaw
    , isNotIn
    , isNotInRaw
    , between
    , betweenRaw
    , notBetween
    , notBetweenRaw
    , isNull
    , isNotNull
    , like
    , likeRaw
    , notLike
    , notLikeRaw
    , and
    , and_
    , or
    , or_
    , null
    , true
    , false
    , begin
    ) where

import Data.Text as T hiding (null)
import Data.Text (Text)
import Data.Semigroup
import Control.Monad
import Control.Monad.Identity
import qualified Control.Monad.IO.Class as MIO
import Control.Applicative
import Prelude hiding (and, or, null, not)

import qualified QueryBuilder.Internal.Condition as Internal

type QueryCondition = Internal.QueryCondition
type ConditionT m   = Internal.ConditionT QueryCondition m Bool
type ConditionM     = Internal.ConditionT QueryCondition Identity Bool

rawQueryCondition :: Text -> [Text] -> QueryCondition
rawQueryCondition = Internal.Condition

rawQueryConditionT :: (Monad m) => Text -> [Text] -> ConditionT m
rawQueryConditionT a b = Internal.ConditionT $
    return (True, rawQueryCondition a b)

raw :: (Monad m) => Text -> ConditionT m
raw a = rawQueryConditionT a []

runConditionT :: (Monad m) => Internal.ConditionT a m b -> m a
runConditionT q = (return . snd) =<< Internal.runConditionT q

runConditionM :: ConditionM -> QueryCondition
runConditionM = snd . runIdentity . Internal.runConditionT

clause    = Internal.clause
bindings = Internal.bindings

condition :: (Monad m) => Text -> ConditionT m -> ConditionT m
condition left right = Internal.ConditionT $ do
    (_, right') <- Internal.runConditionT right
    let left'  = Internal.Condition left []
    return (True, left' <> right')
{-# INLINE condition #-}

equals :: (Monad m) => Text -> ConditionT m
equals v = Internal.ConditionT $ do
    return (True, Internal.equals v)
{-# INLINE equals #-}

equalsRaw :: (Monad m) => Text -> ConditionT m
equalsRaw v = Internal.ConditionT $ do
    return (True, Internal.equalsRaw v)
{-# INLINE equalsRaw #-}

notEquals :: (Monad m) => Text -> ConditionT m
notEquals v = Internal.ConditionT $ do
    return (True, Internal.notEquals v)
{-# INLINE notEquals #-}

notEqualsRaw :: (Monad m) => Text -> ConditionT m
notEqualsRaw v = Internal.ConditionT $ do
    return (True, Internal.notEqualsRaw v)
{-# INLINE notEqualsRaw #-}

eq :: (Monad m) => Text -> ConditionT m
eq = equals
{-# INLINE eq #-}

eqRaw :: (Monad m) => Text -> ConditionT m
eqRaw = equalsRaw
{-# INLINE eqRaw #-}

neq :: (Monad m) => Text -> ConditionT m
neq = notEquals
{-# INLINE neq #-}

neqRaw :: (Monad m) => Text -> ConditionT m
neqRaw = notEqualsRaw
{-# INLINE neqRaw #-}

gt :: (Monad m) => Text -> ConditionT m
gt v = Internal.ConditionT $ do
    return (True, Internal.greaterThan v)
{-# INLINE gt #-}

gtRaw :: (Monad m) => Text -> ConditionT m
gtRaw v = Internal.ConditionT $ do
    return (True, Internal.greaterThanRaw v)
{-# INLINE gtRaw #-}

gte :: (Monad m) => Text -> ConditionT m
gte v = Internal.ConditionT $ do
    return (True, Internal.greaterThanOrEquals v)
{-# INLINE gte #-}

gteRaw :: (Monad m) => Text -> ConditionT m
gteRaw v = Internal.ConditionT $ do
    return (True, Internal.greaterThanOrEqualsRaw v)
{-# INLINE gteRaw #-}

lt :: (Monad m) => Text -> ConditionT m
lt v = Internal.ConditionT $ do
    return (True, Internal.lessThan v)
{-# INLINE lt #-}

ltRaw :: (Monad m) => Text -> ConditionT m
ltRaw v = Internal.ConditionT $ do
    return (True, Internal.lessThanRaw v)
{-# INLINE ltRaw #-}

lte :: (Monad m) => Text -> ConditionT m
lte v = Internal.ConditionT $ do
    return (True, Internal.lessThanOrEquals v)
{-# INLINE lte #-}

lteRaw :: (Monad m) => Text -> ConditionT m
lteRaw v = Internal.ConditionT $ do
    return (True, Internal.lessThanOrEqualsRaw v)
{-# INLINE lteRaw #-}

is :: (Monad m) => Text -> ConditionT m
is v = Internal.ConditionT $ do
    return (True, Internal.is v)
{-# INLINE is #-}

isRaw :: (Monad m) => Text -> ConditionT m
isRaw v = Internal.ConditionT $ do
    return (True, Internal.isRaw v)
{-# INLINE isRaw #-}

isNot :: (Monad m) => Text -> ConditionT m
isNot v = Internal.ConditionT $ do
    return (True, Internal.isNot v)
{-# INLINE isNot #-}

isNotRaw :: (Monad m) => Text -> ConditionT m
isNotRaw v = Internal.ConditionT $ do
    return (True, Internal.isNotRaw v)
{-# INLINE isNotRaw #-}

not :: (Monad m) => Text -> ConditionT m
not v = Internal.ConditionT $ do
    return (True, Internal.not v)
{-# INLINE not #-}

notRaw :: (Monad m) => Text -> ConditionT m
notRaw v = Internal.ConditionT $ do
    return (True, Internal.notRaw v)
{-# INLINE notRaw #-}

isIn :: (Monad m) => [Text] -> ConditionT m
isIn v = Internal.ConditionT $ do
    return (True, Internal.isIn v)
{-# INLINE isIn #-}

isInRaw :: (Monad m) => [Text] -> ConditionT m
isInRaw v = Internal.ConditionT $ do
    return (True, Internal.isInRaw v)
{-# INLINE isInRaw #-}

isNotIn :: (Monad m) => [Text] -> ConditionT m
isNotIn v = Internal.ConditionT $ do
    return (True, Internal.isNotIn v)
{-# INLINE isNotIn #-}

isNotInRaw :: (Monad m) => [Text] -> ConditionT m
isNotInRaw v = Internal.ConditionT $ do
    return (True, Internal.isNotInRaw v)
{-# INLINE isNotInRaw #-}

between :: (Monad m) => Text -> Text -> ConditionT m
between a c = Internal.ConditionT $ return (True, Internal.between a c)
{-# INLINE between #-}

betweenRaw :: (Monad m) => Text -> Text -> ConditionT m
betweenRaw a c = Internal.ConditionT $ return (True, Internal.betweenRaw a c)
{-# INLINE betweenRaw #-}

notBetween :: (Monad m) => Text -> Text -> ConditionT m
notBetween a c = Internal.ConditionT $ return (True, Internal.notBetween a c)
{-# INLINE notBetween #-}

notBetweenRaw :: (Monad m) => Text -> Text -> ConditionT m
notBetweenRaw a c = Internal.ConditionT $ return (True, Internal.notBetweenRaw a c)
{-# INLINE notBetweenRaw #-}

isNull :: (Monad m) => ConditionT m
isNull = Internal.ConditionT $ return (True, Internal.isNull)
{-# INLINE isNull #-}

isNotNull :: (Monad m) => ConditionT m
isNotNull = Internal.ConditionT $ do
    return (True, Internal.isNotNull)
{-# INLINE isNotNull #-}

like :: (Monad m) => Text -> ConditionT m
like v = Internal.ConditionT $ do
    return(True, Internal.like v)
{-# INLINE like #-}

likeRaw :: (Monad m) => Text -> ConditionT m
likeRaw v = Internal.ConditionT $ do
    return(True, Internal.likeRaw v)
{-# INLINE likeRaw #-}

notLike :: (Monad m) => Text -> ConditionT m
notLike v = Internal.ConditionT $ do
    return(True, Internal.notLike v)
{-# INLINE notLike #-}

notLikeRaw :: (Monad m) => Text -> ConditionT m
notLikeRaw v = Internal.ConditionT $ do
    return(True, Internal.notLikeRaw v)
{-# INLINE notLikeRaw #-}

null = Internal.null
{-# INLINE null #-}

true = Internal.true
{-# INLINE true #-}

false = Internal.false
{-# INLINE false #-}

and :: (Monad m) => Text -> ConditionT m -> ConditionT m
and left right = do
    Internal.ConditionT $ return (False, Internal.and)
    condition left right
{-# INLINE and #-}

and_ :: (Monad m) => ConditionT m -> ConditionT m
and_ = begin and
{-# INLINE and_ #-}

or :: (Monad m) => Text -> ConditionT m -> ConditionT m
or left right = do
    Internal.ConditionT $ return (False, Internal.or)
    condition left right
{-# INLINE or #-}

or_ :: (Monad m) => ConditionT m -> ConditionT m
or_ = begin or
{-# INLINE or_ #-}

-- | basically just ignore the Text of a (Text -> CondT -> CondtT)
begin :: (Monad m) => (Text -> ConditionT m -> ConditionT m) -> ConditionT m -> ConditionT m
begin f c = uncurry f ("", grouped) 
  where
    -- grouped :: (Monad m) => ConditionT m
    grouped = Internal.ConditionT $ do
        let open  = Internal.Condition "(" []
            close = Internal.Condition ")" []
        (_, q) <- Internal.runConditionT c
        return (False, open <> q <> close)
{-# INLINABLE begin #-}

