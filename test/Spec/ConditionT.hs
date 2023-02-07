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
    describe "x" $ do
      context "y" $ do
        it "ConditionT" $ do
          query testConditionTransformer `shouldBe` ""
          bindings testConditionTransformer `shouldBe` []

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
