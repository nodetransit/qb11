{-# LANGUAGE OverloadedStrings #-}

module Spec.Condition
    ( runConditionSpec
    ) where

import Prelude hiding (and, or, null, (&&), (||))
import Data.Text as T hiding (null)
import Test.Hspec

import QueryBuilder.Condition
import QueryBuilder.ToText

runConditionSpec :: Spec
runConditionSpec =
    describe "condition semigroup/monoid" $ do
      context "test concatenation" $ do
        it "simple query condition catenation" $ do
          query testConcatenate `shouldBe` "a = ? AND b IS ? AND ( c <> ? OR d = ? OR d IS ? )"
          bindings testConcatenate `shouldBe` ["1", "NULL", "C", "", "NULL"]
        it "simple query condition catenation" $ do
          query operatorOverload `shouldBe` "a = ? AND b IS NOT ? AND ( c IS NOT ? OR c <> ? ) AND d LIKE ?"
          bindings operatorOverload `shouldBe` ["0", "NULL"]

testConcatenate :: QueryCondition
testConcatenate =
    condition "a" Equals true
    <> and <> condition "b" Is null 
    <> and
    <> begin (
        condition "c" NotEquals "C"
        <> or <> condition "d" Equals ""
        <> or <> condition "d" Is null
    )

operatorOverload :: QueryCondition
operatorOverload =
       condition "a" Equals false
    && condition "b" IsNot null
    &&... ( condition "c" IsNot null
         || condition "c" NotEquals ""
    )
    && condition "d" Like "%D%"

