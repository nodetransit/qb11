{-# LANGUAGE OverloadedStrings #-}

module Spec.Operators
    ( operatorSpec
    ) where

import Prelude hiding (and, or, null, not, (&&), (||))
import Data.Text as T hiding (null)
import Test.Hspec
import Control.Monad.Identity

import QueryBuilder.Condition

operatorSpec :: Spec
operatorSpec =
    describe "condition clause" $ do
      context "operator strings" $ do

        it "equals" $ do
          query runEquals `shouldBe` "a = ?"
          bindings runEquals `shouldBe` ["A"]

        it "not equals" $ do
          query runNotEquals `shouldBe` "b <> ?"
          bindings runNotEquals `shouldBe` ["C"]

        it "is" $ do
          query runIs `shouldBe` "this IS ?"
          bindings runIs `shouldBe` ["that"]

        it "is not" $ do
          query runIsNot `shouldBe` "that IS NOT ?"
          bindings runIsNot `shouldBe` ["this"]

        it "not" $ do
          query runNot `shouldBe` "x NOT ?"
          bindings runNot `shouldBe` ["y"]

        it "is null" $ do
          query runIsNull `shouldBe` "empty IS NULL"
          bindings runIsNull `shouldBe` []

        it "is not null" $ do
          query runIsNotNull `shouldBe` "something IS NOT NULL"
          bindings runIsNotNull `shouldBe` []

        it "like" $ do
          query runLike `shouldBe` "girl LIKE ?"
          bindings runLike `shouldBe` ["%akane%"]

        it "not like" $ do
          query runNotLike `shouldBe` "maybe NOT LIKE ?"
          bindings runNotLike `shouldBe` ["%this%"]

        it "and" $ do
          query runAnd `shouldBe` "AND d = ?"
          bindings runAnd `shouldBe` ["D"]

        it "or" $ do
          query runOr `shouldBe` "OR e = ?"
          bindings runOr `shouldBe` ["E"]

        it "constants" $ do
          getNull `shouldBe` "NULL"
          getTrue `shouldBe` "1"
          getFalse `shouldBe` "0"

runEquals :: QueryCondition
runEquals = (runIdentity . runConditionT) $ condition "a" (equals "A")

runNotEquals :: QueryCondition
runNotEquals = (runIdentity . runConditionT) $ condition "b" (notEquals "C")

runIs :: QueryCondition
runIs = (runIdentity . runConditionT) $ condition "this" (is "that")

runIsNot :: QueryCondition
runIsNot = (runIdentity . runConditionT) $ condition "that" (isNot "this")

runNot :: QueryCondition
runNot = (runIdentity . runConditionT) $ condition "x" (not "y")

runIsNull :: QueryCondition
runIsNull = (runIdentity . runConditionT) $ condition "empty" isNull

runIsNotNull :: QueryCondition
runIsNotNull = (runIdentity . runConditionT) $ condition "something" isNotNull

runLike :: QueryCondition
runLike = (runIdentity . runConditionT) $ condition "girl" (like "%akane%")

runNotLike :: QueryCondition
runNotLike = (runIdentity . runConditionT) $ condition "maybe" (notLike "%this%")

runAnd :: QueryCondition
runAnd = (runIdentity . runConditionT) $ and "d" (equals "D")

runOr :: QueryCondition
runOr = (runIdentity . runConditionT) $ or "e" (equals "E")

getNull :: Text
getNull = null

getTrue :: Text
getTrue = true

getFalse :: Text
getFalse = false

