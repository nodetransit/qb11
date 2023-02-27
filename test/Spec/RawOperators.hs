{-# LANGUAGE OverloadedStrings #-}

module Spec.RawOperators
    ( rawOperatorSpec
    ) where

import Prelude hiding (and, or, null, not, (&&), (||))
import Data.Text as T hiding (null)
import Test.Hspec
import Control.Monad.Identity

import QueryBuilder.Condition

rawOperatorSpec :: Spec
rawOperatorSpec =
    describe "raw condition clause" $ do
      context "operator strings" $ do

        it "equals" $ do
          let q = runEqualsRaw
          clause q `shouldBe` "a = A"
          bindings q `shouldBe` mempty

        it "eq" $ do
          let q = runEqRaw
          clause q `shouldBe` "aa = AA"
          bindings q `shouldBe` mempty

        it "not equals" $ do
          let q = runNotEqualsRaw
          clause q `shouldBe` "b <> B"
          bindings q `shouldBe` mempty

        it "neq" $ do
          let q = runNeqRaw
          clause q `shouldBe` "bb <> BB"
          bindings q `shouldBe` mempty

        it "gt" $ do
          let q = runGtRaw
          clause q `shouldBe` "cc > CC"
          bindings q `shouldBe` mempty

        it "gte" $ do
          let q = runGteRaw
          clause q `shouldBe` "dd >= DD"
          bindings q `shouldBe` mempty

        it "lt" $ do
          let q = runLtRaw
          clause q `shouldBe` "ee < EE"
          bindings q `shouldBe` mempty

        it "lte" $ do
          let q = runLteRaw
          clause q `shouldBe` "ff <= FF"
          bindings q `shouldBe` mempty

        it "is" $ do
          let q = runIsRaw
          clause q `shouldBe` "'Akane' IS 'Love'"
          bindings q `shouldBe` mempty

        it "is not" $ do
          let q = runIsNotRaw
          clause q `shouldBe` "'Akane' IS NOT 'Forever'"
          bindings q `shouldBe` mempty

        it "not" $ do
          let q = runNotRaw
          clause q `shouldBe` "a NOT b"
          bindings q `shouldBe` mempty

        it "is in" $ do
          let q = runIsInRaw
          clause q `shouldBe` "language IN (c++, haskell)"
          bindings q `shouldBe` mempty

        it "is not in" $ do
          let q = runIsNotInRaw
          clause q `shouldBe` "language NOT IN (php, java, c#)"
          bindings q `shouldBe` mempty

        it "is between" $ do
          let q = runBetweenRaw
          clause q `shouldBe` "'Kirsthy' BETWEEN A AND C"
          bindings q `shouldBe` mempty

        it "is not between" $ do
          let q = runNotBetweenRaw
          clause q `shouldBe` "d NOT BETWEEN a AND c"
          bindings q `shouldBe` mempty

        it "like" $ do
          let q = runLikeRaw
          clause q `shouldBe` "type LIKE %admin%"
          bindings q `shouldBe` mempty

        it "not like" $ do
          let q = runNotLikeRaw
          clause q `shouldBe` "user NOT LIKE %guest%"
          bindings q `shouldBe` mempty

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

