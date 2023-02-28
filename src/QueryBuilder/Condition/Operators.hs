{-# LANGUAGE OverloadedStrings #-}

module QueryBuilder.Condition.Operators
    ( (&&)
    , (&&...)
    , (||)
    , (||...)
    ) where

import Data.Text as T
import Data.Text (Text)
import Data.Semigroup
import Control.Monad
import Prelude hiding ((&&), (||))

import qualified QueryBuilder.Internal.Condition as Internal
import           QueryBuilder.Condition

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

