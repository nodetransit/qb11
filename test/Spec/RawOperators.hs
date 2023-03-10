{-# LANGUAGE OverloadedStrings #-}

module Spec.RawOperators
    ( rawOperatorSpec
    ) where

import Prelude hiding (and, or, null, not, (&&), (||))
import Data.Text as T hiding (null)
import Data.Semigroup
import Test.Hspec
import Control.Monad.Identity

import QueryBuilder.Condition

rawOperatorSpec :: Spec
rawOperatorSpec =
    describe "raw condition clause" $ do
      context "operator strings" $ do

        it "equals" $ do
          let q = runEqualsRaw
          condition_clause q `shouldBe` "a = A"
          condition_bindings q `shouldBe` mempty

        it "eq" $ do
          let q = runEqRaw
          condition_clause q `shouldBe` "aa = AA"
          condition_bindings q `shouldBe` mempty

        it "not equals" $ do
          let q = runNotEqualsRaw
          condition_clause q `shouldBe` "b <> B"
          condition_bindings q `shouldBe` mempty

        it "neq" $ do
          let q = runNeqRaw
          condition_clause q `shouldBe` "bb <> BB"
          condition_bindings q `shouldBe` mempty

        it "gt" $ do
          let q = runGtRaw
          condition_clause q `shouldBe` "cc > CC"
          condition_bindings q `shouldBe` mempty

        it "gte" $ do
          let q = runGteRaw
          condition_clause q `shouldBe` "dd >= DD"
          condition_bindings q `shouldBe` mempty

        it "lt" $ do
          let q = runLtRaw
          condition_clause q `shouldBe` "ee < EE"
          condition_bindings q `shouldBe` mempty

        it "lte" $ do
          let q = runLteRaw
          condition_clause q `shouldBe` "ff <= FF"
          condition_bindings q `shouldBe` mempty

        it "is" $ do
          let q = runIsRaw
          condition_clause q `shouldBe` "'Akane' IS 'Love'"
          condition_bindings q `shouldBe` mempty

        it "is not" $ do
          let q = runIsNotRaw
          condition_clause q `shouldBe` "'Akane' IS NOT 'Forever'"
          condition_bindings q `shouldBe` mempty

        it "not" $ do
          let q = runNotRaw
          condition_clause q `shouldBe` "a NOT b"
          condition_bindings q `shouldBe` mempty

        it "is in" $ do
          let q = runIsInRaw
          condition_clause q `shouldBe` "language IN (c++, haskell)"
          condition_bindings q `shouldBe` mempty

        it "is not in" $ do
          let q = runIsNotInRaw
          condition_clause q `shouldBe` "language NOT IN (php, java, c#)"
          condition_bindings q `shouldBe` mempty

        it "is between" $ do
          let q = runBetweenRaw
          condition_clause q `shouldBe` "'Kirsthy' BETWEEN A AND C"
          condition_bindings q `shouldBe` mempty

        it "is not between" $ do
          let q = runNotBetweenRaw
          condition_clause q `shouldBe` "d NOT BETWEEN a AND c"
          condition_bindings q `shouldBe` mempty

        it "like" $ do
          let q = runLikeRaw
          condition_clause q `shouldBe` "type LIKE %admin%"
          condition_bindings q `shouldBe` mempty

        it "not like" $ do
          let q = runNotLikeRaw
          condition_clause q `shouldBe` "user NOT LIKE %guest%"
          condition_bindings q `shouldBe` mempty

runEqualsRaw :: QueryCondition
runEqualsRaw = (runIdentity . runConditionT) $ condition "a" (equalsRaw "A")

runEqRaw :: QueryCondition
runEqRaw = (runIdentity . runConditionT) $ condition "aa" (eqRaw "AA")

runNotEqualsRaw :: QueryCondition
runNotEqualsRaw = (runIdentity . runConditionT) $ condition "b" (notEqualsRaw "B")

runNeqRaw :: QueryCondition
runNeqRaw = (runIdentity . runConditionT) $ condition "bb" (neqRaw "BB")

runGtRaw :: QueryCondition
runGtRaw = (runIdentity . runConditionT) $ condition "cc" (gtRaw "CC")

runGteRaw :: QueryCondition
runGteRaw = (runIdentity . runConditionT) $ condition "dd" (gteRaw "DD")

runLtRaw :: QueryCondition
runLtRaw = (runIdentity . runConditionT) $ condition "ee" (ltRaw "EE")

runLteRaw :: QueryCondition
runLteRaw = (runIdentity . runConditionT) $ condition "ff" (lteRaw "FF")

runIsRaw :: QueryCondition
runIsRaw = (runIdentity . runConditionT) $ condition "'Akane'" (isRaw "'Love'")

runIsNotRaw :: QueryCondition
runIsNotRaw = (runIdentity . runConditionT) $ condition "'Akane'" (isNotRaw "'Forever'")

runNotRaw :: QueryCondition
runNotRaw = (runIdentity . runConditionT) $ condition "a" (notRaw "b")

runIsInRaw :: QueryCondition
runIsInRaw = (runIdentity . runConditionT) $ condition "language" (isInRaw ["c++", "haskell"])

runIsNotInRaw :: QueryCondition
runIsNotInRaw = (runIdentity . runConditionT) $ condition "language" (isNotInRaw ["php", "java", "c#"])

runBetweenRaw :: QueryCondition
runBetweenRaw = (runIdentity . runConditionT) $ condition "'Kirsthy'" (betweenRaw "A" "C")

runNotBetweenRaw :: QueryCondition
runNotBetweenRaw = (runIdentity . runConditionT) $ condition "d" (notBetweenRaw "a" "c")

runLikeRaw :: QueryCondition
runLikeRaw = (runIdentity . runConditionT) $ condition "type" (likeRaw "%admin%")

runNotLikeRaw :: QueryCondition
runNotLikeRaw = (runIdentity . runConditionT) $ condition "user" (notLikeRaw "%guest%")

