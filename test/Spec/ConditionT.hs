{-# LANGUAGE OverloadedStrings #-}

module Spec.ConditionT
    ( conditionTSpec
    ) where

import Prelude hiding (and, or, null, (&&), (||))
import Data.Text as T hiding (null)
import Test.Hspec
import Control.Monad
import Control.Monad.Identity

import QueryBuilder.Condition

conditionTSpec :: Spec
conditionTSpec =
    describe "condition transformer" $ do
      context "simple query" $ do
        it "using identity monad" $ do
          query testConditionTransformer `shouldBe` "a = ? AND b IS NULL AND ( c <> ? OR c IS NOT NULL ) AND d LIKE ?"
          bindings testConditionTransformer `shouldBe` ["1", "", "%D%"]

        it "using maybe monad" $ do
          query testConditionTransformerMaybe `shouldBe` "a = ? AND b IS NULL"
          bindings testConditionTransformerMaybe `shouldBe` ["A"]

testConditionTransformer :: QueryCondition
testConditionTransformer = (runIdentity .runConditionT) createQueryCondition
  where
    createQueryCondition :: ConditionT Identity
    createQueryCondition = do
        condition "a" (equals true)
        and "b" isNull
        and `begin` do
            condition "c" (notEquals "")
            or "c" isNotNull
        and "d" (like "%D%")

testConditionTransformerMaybe :: QueryCondition
testConditionTransformerMaybe = (runMaybe . runConditionT) createMaybeQueryCondition
  where
    runMaybe :: Maybe QueryCondition -> QueryCondition
    runMaybe (Just x) = x
    runMaybe _      = rawQueryCondition "" []

    createMaybeQueryCondition :: ConditionT Maybe
    createMaybeQueryCondition = do
        input <- lift getInput
        condition "a" (equals input)
        and "b" isNull

    getInput :: Maybe Text
    getInput = Just "A"

{-
testConditionOperatorTransformer :: QueryCondition
testConditionOperatorTransformer = (runIdentity .runConditionT) createQueryCondition
  where
    createQueryCondition :: ConditionT Identity
    createQueryCondition = do
           condition "a" (equals true)
        && condition "b" isNull
        && condition `begin` do
            condition "c" (notEquals "")
            or "c" isNotNull
        and "d" (like "%D%")

testConditionTransformerMaybe :: QueryCondition
testConditionTransformerMaybe = (runMaybe . runConditionT) createMaybeQueryCondition
  where
    runMaybe :: Maybe QueryCondition -> QueryCondition
    runMaybe (Just x) = x
    runMaybe _      = rawQueryCondition "" []

    createMaybeQueryCondition :: ConditionT Maybe
    createMaybeQueryCondition = do
        input <- lift getInput
        condition "a" (equals input)

    getInput :: Maybe Text
    getInput = Just "A"
-}

