{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}


module QueryBuilder.Condition
    ( ConditionT(..)
    , Condition(..)
    , runConditionT
    , condition
    , QueryCondition
    , Operator(..)
    , equals
    , notEquals
    , isNull
    , isNotNull
    , like
    , and
    , (&&)
    , (&&...)
    , (||)
    , (||...)
    , or
    , null
    , true
    , false
    , begin
    ) where

import Data.Text as T hiding (null)
import Data.Text (Text)
import Control.Monad
import Control.Applicative
import Prelude hiding (and, or, null, (&&), (||))

-- data Condition = Condition Text Operation Text
--             -- | GroupStart
--             -- | GroupEnd

data Operator = Equals
               | NotEquals
               | Is
               | IsNot
               | Not
            -- | Null
            -- | NotNull
               | Like
               | NotLike

instance Show Operator where
    show Equals    = "="
    show NotEquals = "<>"
    show Is        = "IS"
    show IsNot     = "IS NOT"
    show Not       = "NOT"
    -- show Null      = "IS NULL"
    -- show NotNull   = "IS NOT NULL"
    show Like      = "LIKE"
    show NotLike   = "NOT LIKE"

equals :: Text -> QueryCondition
equals v = Condition "= ?" [v]

notEquals :: Text -> QueryCondition
notEquals v = Condition "<> ?" [v]

isNull :: QueryCondition
isNull = Condition "IS NULL" []

isNotNull :: QueryCondition
isNotNull = Condition "IS NOT NULL" []

isNot :: Text -> QueryCondition
isNot v = Condition "IS NOT ?" [v]

like :: Text -> QueryCondition
like v = Condition "LIKE ?" [v]

data
    -- (Monoid query, Monoid bindings) =>
    Condition a b = Condition
        { query :: Text
        , bindings :: b
        }
        -- deriving Functor

type QueryCondition = Condition Text [Text]

instance (Monoid a, Monoid b) => Semigroup (Condition a b) where
    (<>) (Condition aL bL) (Condition aR bR) = Condition (aL <> " " <> aR) (bL <> bR)

instance (Monoid a, Monoid b) => Monoid (Condition a b) where
    mempty = Condition mempty mempty

instance Functor (Condition a) where
    fmap :: (b1 -> b2) -> Condition a b1 -> Condition a b2
    fmap f (Condition a b) = Condition a (f b)

-- instance Applicative (Condition a) where
--     pure = Condition (mempty, mempty)

-- instance Monad (Condition a) where
--     return a = Condition a' []
--       where
--         a' = "(" <> a <> ")"
-- 
--     -- (>>=) :: Condition a b -> (a -> Condition a b) -> Condition a b
--     -- (>>=) c f = Condition q' b'
--     --   where
--     --     c' = f $ query c
--     --     q' = query c'
--     --     b' = bindings c <> bindings c'

condition :: Text -> QueryCondition -> QueryCondition
condition left right = Condition left [] <> right

data InternalConditionT a m b = InternalConditionT { runInternalConditionT :: m (b, a) }

instance (Functor m) => Functor (InternalConditionT a m) where
    fmap f = mapConditionT $ fmap $ \(a, w) -> (f a, w)
      where
        mapConditionT f m = InternalConditionT $ f (runInternalConditionT m)

instance (Monoid a, Applicative m) => Applicative (InternalConditionT a m) where
    pure a = InternalConditionT $ pure (a, mempty)

    (<*>) f v = InternalConditionT $ do
        liftA2 k f' v'
      where
        k (b, a) (b', a') = (b b', a <> a')
        f' = runInternalConditionT f
        v' = runInternalConditionT v
 
instance (Monoid a, Monad m) => Monad (InternalConditionT a m) where
    return :: b -> InternalConditionT a m b
    -- return b = InternalConditionT $ \b -> return (b, mempty)
    return b = (InternalConditionT . return) (b, mempty)

    -- (>>=) :: InternalConditionT a m b -> (a -> InternalConditionT a m b) -> InternalConditionT a m b
    (>>=) m k = InternalConditionT $ do
        (b, a)   <- runInternalConditionT m
        (b', a') <- runInternalConditionT (k b)
        return (b', a <> a')

type ConditionT m = InternalConditionT QueryCondition m Bool

runConditionT :: (Monad m) => InternalConditionT a m b -> m b
runConditionT q = do
    (r, _) <- runInternalConditionT q
    return r

and :: QueryCondition
and = Condition "AND" []
{-# INLINABLE and #-}

or :: QueryCondition
or = Condition "OR" []
{-# INLINABLE or #-}

(&&) :: QueryCondition -> QueryCondition -> QueryCondition
(&&) cL cR = cL <> and <> cR

(||) :: QueryCondition -> QueryCondition -> QueryCondition
(||) cL cR = cL <> or <> cR

null :: Text
null = "NULL"

true :: Text
true = "1"

false :: Text
false = "0"

begin ::QueryCondition -> QueryCondition
begin c = Condition "(" [] <> c <> Condition ")" []
{-# INLINABLE begin #-}

(&&...) :: QueryCondition -> QueryCondition -> QueryCondition
(&&...) cL cR = cL <> and <> begin cR

(||...) :: QueryCondition -> QueryCondition -> QueryCondition
(||...) cL cR = cL <> or <> begin cR

