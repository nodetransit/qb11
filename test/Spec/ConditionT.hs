{-# LANGUAGE OverloadedStrings #-}

module Spec.ConditionT
    ( conditionTSpec
    ) where

import Prelude hiding (and, or, null, (&&), (||))
import Data.Text as T hiding (null)
import Test.Hspec
import Control.Monad.Identity

import QueryBuilder.Condition

conditionTSpec :: Spec
conditionTSpec =
    describe "condition transformer" $ do
      context "simple query" $ do
        it "using identity monad" $ do
          query testConditionTransformer `shouldBe` "a = ? AND b IS NULL AND ( c <> ? OR c IS NOT NULL ) AND d LIKE ?"
          bindings testConditionTransformer `shouldBe` ["1", "", "%D%"]

testConditionTransformer :: QueryCondition
testConditionTransformer = (runIdentity .runConditionT) createQueryCondition
  where
    createQueryCondition :: ConditionT Identity
    createQueryCondition = do
        condition "a" (equals true)
        and "b" isNull
        -- and `uncurry` mkTuple "" xx
        and `begin` do
            condition "c" (notEquals "")
            or "c" isNotNull
        and "d" (like "%D%")
