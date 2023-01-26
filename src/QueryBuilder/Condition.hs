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
import Data.Functor.Identity
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

data ConditionT m b = ConditionT { runConditionT :: m b }

instance (Functor m) => Functor (ConditionT m) where
    fmap :: (a -> b) -> ConditionT m a -> ConditionT m b
    fmap f = mapConditionT $ fmap $ \b -> f b
      where
        mapConditionT f m = ConditionT $ f (runConditionT m)

instance (Applicative m) => Applicative (ConditionT m) where
    pure :: b -> ConditionT m b
    pure b = ConditionT $ pure b

    (<*>) :: ConditionT m (a -> b) -> ConditionT m a -> ConditionT m b
    (<*>) f v = ConditionT $ do
        f' <*> v'
      where
        f' = runConditionT f
        v' = runConditionT v

instance (Monad m) => Monad (ConditionT m) where
    return :: b -> ConditionT m b
    -- return b = ConditionT $ \b -> return (b, mempty)
    return b = (ConditionT . return) b

    -- (>>=) :: ConditionT m b -> (b -> ConditionT m b) -> ConditionT m b
    (>>=) m k = ConditionT $ do
        b <- runConditionT m
        b' <- runConditionT (k b)
        return b'

-- and :: Condition -> Text -> W Text
-- and (Condition a b c) d = W (d <> " " <> a <> (T.pack $ show b) <> "?", [c])

main :: ConditionT Identity (Text, [Text])
main = do
    return ("?", ["a"])
--    f
--    and $ Condition "id" Equals "1"
--  where
--    f = return "where"

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

