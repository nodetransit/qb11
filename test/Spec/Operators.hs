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
          condition_clause runEquals `shouldBe` "a = ?"
          condition_bindings runEquals `shouldBe` ["A"]
          condition_clause runEq `shouldBe` "aa = ?"
          condition_bindings runEq `shouldBe` ["AA"]

        it "not equals" $ do
          condition_clause runNotEquals `shouldBe` "b <> ?"
          condition_bindings runNotEquals `shouldBe` ["C"]
          condition_clause runNotEq `shouldBe` "bb <> ?"
          condition_bindings runNotEq `shouldBe` ["CC"]

        it "greater than" $ do
          let q = runGreaterThan
          condition_clause q `shouldBe` "1 > ?"
          condition_bindings q `shouldBe` ["2"]

        it "greater than or equals" $ do
          let q = runGreaterThanOrEquals
          condition_clause q `shouldBe` "10 >= ?"
          condition_bindings q `shouldBe` ["20"]

        it "less than" $ do
          let q = runLessThan
          condition_clause q `shouldBe` "100 < ?"
          condition_bindings q `shouldBe` ["200"]

        it "less than or equals" $ do
          let q = runLessThanOrEquals
          condition_clause q `shouldBe` "1000 <= ?"
          condition_bindings q `shouldBe` ["2000"]

        it "is" $ do
          condition_clause runIs `shouldBe` "this IS ?"
          condition_bindings runIs `shouldBe` ["that"]

        it "is not" $ do
          condition_clause runIsNot `shouldBe` "that IS NOT ?"
          condition_bindings runIsNot `shouldBe` ["this"]

        it "not" $ do
          condition_clause runNot `shouldBe` "x NOT ?"
          condition_bindings runNot `shouldBe` ["y"]

        it "is in" $ do
          condition_clause runIsIn `shouldBe` "heart IN (?, ?, ?)"
          condition_bindings runIsIn `shouldBe` ["akane", "ayumi", "ayami"]

        it "is not in" $ do
          condition_clause runIsNotIn `shouldBe` "money NOT IN (?, ?, ?)"
          condition_bindings runIsNotIn `shouldBe` ["pocket", "wallet", "bag"]

        it "is between" $ do
          condition_clause runBetween `shouldBe` "ur BETWEEN ? AND ?"
          condition_bindings runBetween `shouldBe` ["ch", "ch"]

        it "is not between" $ do
          condition_clause runNotBetween `shouldBe` "me NOT BETWEEN ? AND ?"
          condition_bindings runNotBetween `shouldBe` ["devil", "ocean"]

        it "is null" $ do
          condition_clause runIsNull `shouldBe` "empty IS NULL"
          condition_bindings runIsNull `shouldBe` []

        it "is not null" $ do
          condition_clause runIsNotNull `shouldBe` "something IS NOT NULL"
          condition_bindings runIsNotNull `shouldBe` []

        it "like" $ do
          condition_clause runLike `shouldBe` "girl LIKE ?"
          condition_bindings runLike `shouldBe` ["%akane%"]

        it "not like" $ do
          condition_clause runNotLike `shouldBe` "maybe NOT LIKE ?"
          condition_bindings runNotLike `shouldBe` ["%this%"]

        it "and" $ do
          condition_clause runAnd `shouldBe` "AND d = ?"
          condition_bindings runAnd `shouldBe` ["D"]

        it "or" $ do
          condition_clause runOr `shouldBe` "OR e = ?"
          condition_bindings runOr `shouldBe` ["E"]

        it "constants" $ do
          getNull `shouldBe` "NULL"
          getTrue `shouldBe` "1"
          getFalse `shouldBe` "0"

runEquals :: QueryCondition
runEquals = (runIdentity . runConditionT) $ condition "a" (equals "A")

runEq :: QueryCondition
runEq = (runIdentity . runConditionT) $ condition "aa" (eq "AA")

runNotEquals :: QueryCondition
runNotEquals = (runIdentity . runConditionT) $ condition "b" (notEquals "C")

runNotEq :: QueryCondition
runNotEq = (runIdentity . runConditionT) $ condition "bb" (neq "CC")

runGreaterThan :: QueryCondition
runGreaterThan = (runIdentity . runConditionT) $ condition "1" (gt "2")

runGreaterThanOrEquals :: QueryCondition
runGreaterThanOrEquals = (runIdentity . runConditionT) $ condition "10" (gte "20")

runLessThan :: QueryCondition
runLessThan = (runIdentity . runConditionT) $ condition "100" (lt "200")

runLessThanOrEquals :: QueryCondition
runLessThanOrEquals = (runIdentity . runConditionT) $ condition "1000" (lte "2000")

runIs :: QueryCondition
runIs = (runIdentity . runConditionT) $ condition "this" (is "that")

runIsNot :: QueryCondition
runIsNot = (runIdentity . runConditionT) $ condition "that" (isNot "this")

runNot :: QueryCondition
runNot = (runIdentity . runConditionT) $ condition "x" (not "y")

runIsIn :: QueryCondition
runIsIn = (runIdentity . runConditionT) $ condition "heart" (isIn ["akane", "ayumi", "ayami"])

runIsNotIn :: QueryCondition
runIsNotIn = (runIdentity . runConditionT) $ condition "money" (isNotIn ["pocket", "wallet", "bag"])

runBetween :: QueryCondition
runBetween = (runIdentity . runConditionT) $ condition "ur" (between "ch" "ch")

runNotBetween :: QueryCondition
runNotBetween = (runIdentity . runConditionT) $ condition "me" (notBetween "devil" "ocean")

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

