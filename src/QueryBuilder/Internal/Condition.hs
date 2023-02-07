{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}


module QueryBuilder.Internal.Condition
    ( ConditionT(..)
    , Condition(..)
    , condition
    , QueryCondition
    , equals
    , notEquals
    , is
    , not
    , isNot
    , isNull
    , isNotNull
    , like
    , notLike
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
import Prelude hiding (and, or, null, not, (&&), (||))

data
    -- (Monoid query, Monoid bindings) =>
    Condition a b = Condition
        { query :: Text
        , bindings :: b
        }
        deriving Show
        -- deriving Functor

type QueryCondition = Condition Text [Text]

instance (Monoid a, Monoid b) => Semigroup (Condition a b) where
    (<>) (Condition aL bL) (Condition aR bR) = Condition (aL <> " " <> aR) (bL <> bR)

instance (Monoid a, Monoid b) => Monoid (Condition a b) where
    mempty = Condition mempty mempty

-- instance Functor (Condition a) where
--     fmap :: (b1 -> b2) -> Condition a b1 -> Condition a b2
--     fmap f (Condition a b) = Condition a (f b)
-- 
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

data ConditionT a m b = ConditionT { runConditionT :: m (b, a) }

instance (Functor m) => Functor (ConditionT a m) where
    fmap f = mapConditionT $ fmap $ \(a, w) -> (f a, w)
      where
        mapConditionT f m = ConditionT $ f (runConditionT m)

instance (Monoid a, Applicative m) => Applicative (ConditionT a m) where
    pure a = ConditionT $ pure (a, mempty)

    (<*>) f v = ConditionT $ do
        liftA2 k f' v'
      where
        k (b, a) (b', a') = (b b', a <> a')
        f' = runConditionT f
        v' = runConditionT v
 
instance (Monoid a, Monad m) => Monad (ConditionT a m) where
    return :: b -> ConditionT a m b
    -- return b = ConditionT $ \b -> return (b, mempty)
    return b = do
        (ConditionT . return) (b, mempty)

    -- (>>=) :: ConditionT a m b -> (a -> ConditionT a m b) -> ConditionT a m b
    (>>=) m k = ConditionT $ do
        (b, a)   <- runConditionT m
        (b', a') <- runConditionT (k b)
        return (b', a <> a')

condition :: Text -> QueryCondition -> QueryCondition
condition left right = Condition left [] <> right

equals :: Text -> QueryCondition
equals v = Condition "= ?" [v]

notEquals :: Text -> QueryCondition
notEquals v = Condition "<> ?" [v]

isNull :: QueryCondition
isNull = Condition "IS NULL" []
{-# INLINABLE isNull #-}

isNotNull :: QueryCondition
isNotNull = Condition "IS NOT NULL" []
{-# INLINABLE isNotNull #-}

is :: Text -> QueryCondition
is v = Condition "IS ?" [v]
{-# INLINABLE is #-}

not :: Text -> QueryCondition
not v = Condition "NOT ?" [v]
{-# INLINABLE not #-}

isNot :: Text -> QueryCondition
isNot v = Condition "IS NOT ?" [v]
{-# INLINABLE isNot #-}

like :: Text -> QueryCondition
like v = Condition "LIKE ?" [v]
{-# INLINABLE like #-}

notLike :: Text -> QueryCondition
notLike v = Condition "NOT LIKE ?" [v]
{-# INLINABLE notLike #-}

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
{-# INLINE null #-}

true :: Text
true = "1"
{-# INLINE true #-}

false :: Text
false = "0"
{-# INLINE false #-}

begin ::QueryCondition -> QueryCondition
begin c = Condition "(" [] <> c <> Condition ")" []
{-# INLINABLE begin #-}

(&&...) :: QueryCondition -> QueryCondition -> QueryCondition
(&&...) cL cR = cL <> and <> begin cR

(||...) :: QueryCondition -> QueryCondition -> QueryCondition
(||...) cL cR = cL <> or <> begin cR

