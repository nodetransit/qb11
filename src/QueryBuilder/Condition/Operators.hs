{-# LANGUAGE OverloadedStrings #-}

module QueryBuilder.Condition.Operators
    ( (&&)
    , (&&...)
    , (||)
    , (||...)
    ) where

import Data.Text as T
import Data.Text (Text)
import Control.Monad
import Prelude hiding ((&&), (||))

import qualified QueryBuilder.Internal.Condition as Internal
import           QueryBuilder.Condition

-- data Operator = Equals
--                | NotEquals
--                | Is
--                | IsNot
--                | Not
--              - | Null
--                | NotNull
--                | Like
--                | NotLike
--
-- instance Show Operator where
--     show Equals    = "="
--     show NotEquals = "<>"
--     show Is        = "IS"
--     show IsNot     = "IS NOT"
--     show Not       = "NOT"
--     show IsNull    = "IS NULL"
--     show IsNotNull = "IS NOT NULL"
--     show Like      = "LIKE"
--     show NotLike   = "NOT LIKE"

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

