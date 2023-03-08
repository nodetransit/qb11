{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -Wno-missing-fields #-}

module Spec.Alias
    ( aliasSpec
    ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Semigroup
import Spec.Util

import QueryBuilder.Alias

aliasSpec :: Spec
aliasSpec =
  describe "alias" $ do
    context "alias overload" $ do
      it "create alias from string" $
        testAliasOverload `shouldBe` As "maya"

-- |
testAliasOverload :: Alias
testAliasOverload = "maya"
