{-# LANGUAGE OverloadedStrings #-}

module Spec.Condition
    ( conditionSpec
    ) where

import Prelude hiding (and, or, null, (&&), (||))
import Data.Text as T hiding (null)
import Test.Hspec
import Control.Monad.Identity

import QueryBuilder.Internal.Condition
import QueryBuilder.ToText

conditionSpec :: Spec
conditionSpec =
    describe "condition semigroup/monoid" $ do
      context "simple query concatenation" $ do
        it "using mconcat" $ do
          clause testConcatenate `shouldBe` "a = ? AND b IS NULL AND ( c <> ? OR d = ? OR d IS NULL ) AND e = E"
          bindings testConcatenate `shouldBe` ["1", "C", ""]

        it "using operators" $ do
          clause operatorOverload `shouldBe` "a = ? AND b IS NOT NULL AND ( c IS NOT NULL OR c <> ? ) AND d LIKE ?"
          bindings operatorOverload `shouldBe` ["0", "", "%D%"]

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
    <> and <> rawCondition "e = E"

operatorOverload :: QueryCondition
operatorOverload =
       condition "a" (equals false)
    && condition "b" isNotNull
    &&... ( condition "c" isNotNull
         || condition "c" (notEquals "")
    )
    && condition "d" (like "%D%")

