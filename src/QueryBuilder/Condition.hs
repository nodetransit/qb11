{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}


module QueryBuilder.Condition
    ( ConditionT
    , Condition(..)
    , runConditionT
    , lift
    , liftIO
    , condition
    , QueryCondition
    , rawQueryCondition
    , rawQueryConditionT
    , query
    , bindings
    , equals
    , notEquals
    , is
    , isNot
    , not
    , isNull
    , isNotNull
    , like
    , notLike
    , and
    , or
    , (&&)
    , (&&...)
    , (||)
    , (||...)
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
import Prelude hiding (and, or, null, not, (&&), (||))

import           QueryBuilder.Operator
import qualified QueryBuilder.Internal.Condition as Internal

type QueryCondition = Internal.QueryCondition
type ConditionT m   = Internal.ConditionT QueryCondition m Bool
type Condition      = Internal.ConditionT QueryCondition Identity Bool

rawQueryCondition :: Text -> [Text] -> QueryCondition
rawQueryCondition a b = Internal.Condition a b

rawQueryConditionT :: (Monad m) => Text -> [Text] -> ConditionT m
rawQueryConditionT a b = Internal.ConditionT $ return (True, rawQueryCondition a b)

runConditionT :: (Monad m) => Internal.ConditionT a m b -> m a
runConditionT q = (return . snd) =<< Internal.runConditionT q

lift ::(Monad m) => m a -> Internal.ConditionT QueryCondition m a
lift = Internal.lift
{-# INLINE lift #-}

liftIO :: (MIO.MonadIO m) => IO a -> m a
liftIO = Internal.liftIO
{-# INLINE liftIO #-}

query    = Internal.query
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

notEquals :: (Monad m) => Text -> ConditionT m
notEquals v = Internal.ConditionT $ do
    return (True, Internal.notEquals v)
{-# INLINABLE notEquals #-}

is :: (Monad m) => Text -> ConditionT m
is v = Internal.ConditionT $ do
    return (True, Internal.is v)
{-# INLINABLE is #-}

isNot :: (Monad m) => Text -> ConditionT m
isNot v = Internal.ConditionT $ do
    return (True, Internal.isNot v)
{-# INLINABLE isNot #-}

not :: (Monad m) => Text -> ConditionT m
not v = Internal.ConditionT $ do
    return (True, Internal.not v)
{-# INLINABLE not #-}

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

notLike :: (Monad m) => Text -> ConditionT m
notLike v = Internal.ConditionT $ do
    return(True, Internal.notLike v)
{-# INLINABLE notLike #-}

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

or :: (Monad m) => Text -> ConditionT m -> ConditionT m
or left right = do
    Internal.ConditionT $ return (False, Internal.or)
    condition left right
{-# INLINABLE or #-}

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

infixl 8 &&
(&&) :: (Monad m) => ConditionT m -> ConditionT m -> ConditionT m
(&&) left right = do
    Internal.ConditionT $ do
        (_, l) <- Internal.runConditionT left
        (_, r) <- Internal.runConditionT right
        return (False,  l <> Internal.and <> r)

infixl 8 &&...
(&&...) :: (Monad m) => ConditionT m -> ConditionT m -> ConditionT m
(&&...) left right = Internal.ConditionT $ do
        (_, l) <- Internal.runConditionT left
        (_, r) <- Internal.runConditionT right
        let open  = Internal.Condition "(" []
            close = Internal.Condition ")" []
        return (False,  l <> Internal.and <> open <> r <> close)

infixl 8 ||
(||) :: (Monad m) => ConditionT m -> ConditionT m -> ConditionT m
(||) left right = do
    Internal.ConditionT $ do
        (_, l) <- Internal.runConditionT left
        return (False, l)
    Internal.ConditionT $ return (False, Internal.or)
    Internal.ConditionT $ do
        (_, r) <- Internal.runConditionT right
        return (False, r)

infixl 8 ||...
(||...) :: (Monad m) => ConditionT m -> ConditionT m -> ConditionT m
(||...) left right = Internal.ConditionT $ do
        (_, l) <- Internal.runConditionT left
        (_, r) <- Internal.runConditionT right
        let open  = Internal.Condition "(" []
            close = Internal.Condition ")" []
        return (False,  l <> Internal.or <> open <> r <> close)

