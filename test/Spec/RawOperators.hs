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

        it "not equals" $ do
          let q = runNotEqualsRaw
          clause q `shouldBe` "b <> B"
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

        it "between" $ do
          let q = runBetweenRaw
          clause q `shouldBe` "'Kirsthy' BETWEEN A AND C"
          bindings q `shouldBe` mempty

        it "not between" $ do
          let q = runNotBetweenRaw
          clause q `shouldBe` "d NOT BETWEEN a AND c"
          bindings q `shouldBe` mempty

runEqualsRaw :: QueryCondition
runEqualsRaw = (runIdentity . runConditionT) $ condition "a" (equalsRaw "A")

runNotEqualsRaw :: QueryCondition
runNotEqualsRaw = (runIdentity . runConditionT) $ condition "b" (notEqualsRaw "B")

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

