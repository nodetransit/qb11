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
    , lift
    , liftIO
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
    , andBegin
    , or
    , orBegin
    , null
    , true
    , false
    , begin
    ) where

import Data.Text as T hiding (null)
import Data.Text (Text)
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

lift ::(Monad m) => m a -> Internal.ConditionT QueryCondition m a
lift = Internal.lift
{-# INLINE lift #-}

liftIO :: (MIO.MonadIO m) => IO a -> m a
liftIO = Internal.liftIO
{-# INLINE liftIO #-}

clause    = Internal.clause
bindings = Internal.bindings

condition :: (Monad m) => Text -> ConditionT m -> ConditionT m
condition left right = Internal.ConditionT $ do
    (_, right') <- Internal.runConditionT right
    let left'  = Internal.Condition left []
    return (True, left' <> right')
{-# INLINABLE condition #-}

equals :: (Monad m) => Text -> ConditionT m
equals v = Internal.ConditionT $ do
    return (True, Internal.equals v)
{-# INLINABLE equals #-}

equalsRaw :: (Monad m) => Text -> ConditionT m
equalsRaw v = Internal.ConditionT $ do
    return (True, Internal.equalsRaw v)
{-# INLINABLE equalsRaw #-}

notEquals :: (Monad m) => Text -> ConditionT m
notEquals v = Internal.ConditionT $ do
    return (True, Internal.notEquals v)
{-# INLINABLE notEquals #-}

notEqualsRaw :: (Monad m) => Text -> ConditionT m
notEqualsRaw v = Internal.ConditionT $ do
    return (True, Internal.notEqualsRaw v)
{-# INLINABLE notEqualsRaw #-}

is :: (Monad m) => Text -> ConditionT m
is v = Internal.ConditionT $ do
    return (True, Internal.is v)
{-# INLINABLE is #-}

isRaw :: (Monad m) => Text -> ConditionT m
isRaw v = Internal.ConditionT $ do
    return (True, Internal.isRaw v)
{-# INLINABLE isRaw #-}

isNot :: (Monad m) => Text -> ConditionT m
isNot v = Internal.ConditionT $ do
    return (True, Internal.isNot v)
{-# INLINABLE isNot #-}

isNotRaw :: (Monad m) => Text -> ConditionT m
isNotRaw v = Internal.ConditionT $ do
    return (True, Internal.isNotRaw v)
{-# INLINABLE isNotRaw #-}

not :: (Monad m) => Text -> ConditionT m
not v = Internal.ConditionT $ do
    return (True, Internal.not v)
{-# INLINABLE not #-}

notRaw :: (Monad m) => Text -> ConditionT m
notRaw v = Internal.ConditionT $ do
    return (True, Internal.notRaw v)
{-# INLINABLE notRaw #-}

isIn :: (Monad m) => [Text] -> ConditionT m
isIn v = Internal.ConditionT $ do
    return (True, Internal.isIn v)
{-# INLINABLE isIn #-}

isInRaw :: (Monad m) => [Text] -> ConditionT m
isInRaw v = Internal.ConditionT $ do
    return (True, Internal.isInRaw v)
{-# INLINABLE isInRaw #-}

isNotIn :: (Monad m) => [Text] -> ConditionT m
isNotIn v = Internal.ConditionT $ do
    return (True, Internal.isNotIn v)
{-# INLINABLE isNotIn #-}

isNotInRaw :: (Monad m) => [Text] -> ConditionT m
isNotInRaw v = Internal.ConditionT $ do
    return (True, Internal.isNotInRaw v)
{-# INLINABLE isNotInRaw #-}

between :: (Monad m) => Text -> Text -> ConditionT m
between a c = Internal.ConditionT $ return (True, Internal.between a c)
{-# INLINABLE between #-}

betweenRaw :: (Monad m) => Text -> Text -> ConditionT m
betweenRaw a c = Internal.ConditionT $ return (True, Internal.betweenRaw a c)
{-# INLINABLE betweenRaw #-}

notBetween :: (Monad m) => Text -> Text -> ConditionT m
notBetween a c = Internal.ConditionT $ return (True, Internal.notBetween a c)
{-# INLINABLE notBetween #-}

notBetweenRaw :: (Monad m) => Text -> Text -> ConditionT m
notBetweenRaw a c = Internal.ConditionT $ return (True, Internal.notBetweenRaw a c)
{-# INLINABLE notBetweenRaw #-}

isNull :: (Monad m) => ConditionT m
isNull = Internal.ConditionT $ return (True, Internal.isNull)
{-# INLINABLE isNull #-}

isNotNull :: (Monad m) => ConditionT m
isNotNull = Internal.ConditionT $ do
    return (True, Internal.isNotNull)
{-# INLINABLE isNotNull #-}

like :: (Monad m) => Text -> ConditionT m
like v = Internal.ConditionT $ do
    return(True, Internal.like v)
{-# INLINABLE like #-}

likeRaw :: (Monad m) => Text -> ConditionT m
likeRaw v = Internal.ConditionT $ do
    return(True, Internal.likeRaw v)
{-# INLINABLE likeRaw #-}

notLike :: (Monad m) => Text -> ConditionT m
notLike v = Internal.ConditionT $ do
    return(True, Internal.notLike v)
{-# INLINABLE notLike #-}

notLikeRaw :: (Monad m) => Text -> ConditionT m
notLikeRaw v = Internal.ConditionT $ do
    return(True, Internal.notLikeRaw v)
{-# INLINABLE notLikeRaw #-}

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
{-# INLINABLE and #-}

andBegin :: (Monad m) => ConditionT m -> ConditionT m
andBegin = begin and
{-# INLINABLE andBegin #-}

or :: (Monad m) => Text -> ConditionT m -> ConditionT m
or left right = do
    Internal.ConditionT $ return (False, Internal.or)
    condition left right
{-# INLINABLE or #-}

orBegin :: (Monad m) => ConditionT m -> ConditionT m
orBegin = begin or
{-# INLINABLE orBegin #-}

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

