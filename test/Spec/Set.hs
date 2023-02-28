{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-missing-fields #-}

module Spec.Set
    ( setSpec
    ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Spec.Util
import Control.Monad

import QueryBuilder.Set

setSpec :: Spec
setSpec =
  describe "update values" $ do
    context "values and bindings" $ do
      it "should be escaped" $ do
        True `shouldBe` False
      it "should not be escaped" $ do
        True `shouldBe` False
