{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QueryBuilder.Condition
    ( ConditionT(..)
    , Condition(..)
    , condition
    , QueryCondition
    , Operation(..)
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

data Operation = Equals
               | NotEquals
               | Is
               | IsNot
               | Not
            -- | Null
            -- | NotNull
               | Like
               | NotLike

instance Show Operation where
    show Equals    = "="
    show NotEquals = "<>"
    show Is        = "IS"
    show IsNot     = "IS NOT"
    show Not       = "NOT"
    -- show Null      = "IS NULL"
    -- show NotNull   = "IS NOT NULL"
    show Like      = "LIKE"
    show NotLike   = "NOT LIKE"

data
    -- (Monoid query, Monoid bindings) =>
    Condition query bindings = Condition
        { query :: Text
        , bindings :: [Text]
        }

type QueryCondition = Condition Text [Text]

instance (Monoid a, Monoid b) => Semigroup (Condition a b) where
    (<>) (Condition aL bL) (Condition aR bR) = Condition (aL <> " " <> aR) (bL <> bR)

instance (Monoid a, Monoid b) => Monoid (Condition a b) where
    mempty = Condition mempty mempty

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
-- 
-- instance Applicative (Condition a) where
--     pure = return

condition :: Text -> Operation -> Text -> QueryCondition
condition left op right = Condition query bindings
  where
    operation = (T.pack . show) op
    query    = left <> " " <> operation <> " ?"
    bindings = [right]

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
    return b = (ConditionT . return) (b, mempty)

    -- (>>=) :: ConditionT a m b -> (a -> ConditionT a m b) -> ConditionT a m b
    (>>=) m k = ConditionT $ do
        (b, a) <- runConditionT m
        (b', a') <- runConditionT (k b)
        return (b', a <> a')

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

