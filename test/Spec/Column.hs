{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -Wno-missing-fields #-}

module Spec.Column
    ( runColumnSpec
    ) where

import Test.Hspec
import Spec.Util
import QueryBuilder.Query

runColumnSpec :: Spec
runColumnSpec =
  describe "column semigroup/monoid" $ do
    context "concatenation with columns" $ do
      it "should overwrite columns" $ do
        checkConcatColumns select_ `shouldBe` True

-- |
checkConcatColumns :: Query -> Bool
checkConcatColumns q = query_columns q' `isSameColumns` cols
  where
    cols = [Column "id", Column "name"]
    q' = q <> Columns cols

