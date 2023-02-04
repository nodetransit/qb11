{-# LANGUAGE OverloadedStrings #-}

module Spec.Condition
    ( conditionSpec
    ) where

import Prelude hiding (and, or, null, (&&), (||))
import Data.Text as T hiding (null)
import Test.Hspec
import Control.Monad.Identity

import QueryBuilder.Condition
import QueryBuilder.ToText

conditionSpec :: Spec
conditionSpec =
    describe "condition semigroup/monoid" $ do
      context "test concatenation" $ do
        it "simple query condition catenation" $ do
          query testConcatenate `shouldBe` "a = ? AND b IS NULL AND ( c <> ? OR d = ? OR d IS NULL )"
          bindings testConcatenate `shouldBe` ["1", "C", ""]
        it "simple query condition catenation" $ do
          query operatorOverload `shouldBe` "a = ? AND b IS NOT NULL AND ( c IS NOT NULL OR c <> ? ) AND d LIKE ?"
          bindings operatorOverload `shouldBe` ["0", "", "%D%"]
        it "ConditionT" $ do
          testConditionTransformer `shouldBe` True

testConcatenate :: QueryCondition
testConcatenate =
    condition "a" (equals true)
    <> and <> condition "b" isNull 
    <> and
    <> begin (
        condition "c" (notEquals "C")
        <> or <> condition "d" (equals "")
        <> or <> condition "d" isNull
    )

operatorOverload :: QueryCondition
operatorOverload =
       condition "a" (equals false)
    && condition "b" isNotNull
    &&... ( condition "c" isNotNull
         || condition "c" (notEquals "")
    )
    && condition "d" (like "%D%")

testConditionTransformer :: Bool
testConditionTransformer = (runIdentity .runConditionT) createQueryCondition
  where
    createQueryCondition :: ConditionT Identity
    createQueryCondition = do
        return False
