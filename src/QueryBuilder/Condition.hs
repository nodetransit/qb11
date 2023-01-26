{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module QueryBuilder.Condition
    ( ConditionT(..)
    , Condition(..)
    , Operation(..)
    -- , and_
    -- , or_
    -- , main
    ) where

import Data.Text as T
import Data.Text (Text)
import Control.Monad
import Control.Applicative
import Prelude hiding (and)

data Condition = Condition Text Operation Text
            -- | GroupStart
            -- | GroupEnd

data Operation = Equals
               | NotEquals
               | Is
               | Null
               | NotNull
               | Like
               | NotLike

instance Show Operation where
    show Equals    = "="
    show NotEquals = "<>"
    show Is        = "IS"
    show Null      = "iS NULL"
    show NotNull   = "IS NOT NULL"
    show Like      = "LIKE"
    show NotLike   = "NOT LIKE"

data ConditionT a m b = ConditionT { runConditionT :: m (b, a) }

instance (Functor m) => Functor (ConditionT b m) where
    fmap f = mapConditionT $ fmap $ \(b, w) -> (f b, w)
      where
        mapConditionT f m = ConditionT $ f (runConditionT m)

instance (Monoid b, Applicative m) => Applicative (ConditionT b m) where
    pure b = ConditionT $ pure (b, mempty)

    (<*>) f v = ConditionT $ liftA2 k f' v'
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

-- and :: Condition -> Text -> W Text
-- and (Condition a b c) d = W (d <> " " <> a <> (T.pack $ show b) <> "?", [c])
-- 
-- main :: W Text
-- main = do
--     f
--     and $ Condition "id" Equals "1"
--   where
--     f = return "where"

-- and_ :: Bool
-- and_ = True
-- 
-- or_ :: Bool
-- or_ = False
-- 
-- equals_ :: Text -> Text -> (Text, [Text])
-- equals_ column binding = (column, "= ?", [binding])
-- 
-- equals_ :: Text -> Raw Text -> (Text, [Text])
-- equals_ column binding = (column, "= ?", [binding])

