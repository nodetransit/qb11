{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -Wno-missing-fields #-}

module Spec.Column
    ( columnSpec
    ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Spec.Util
import QueryBuilder.Query

columnSpec :: Spec
columnSpec =
  describe "column semigroup/monoid" $ do
    context "concatenation with columns" $ do
      prop "should overwrite columns" $ do
        (query_columns . checkConcatColumns) Select `shouldBeTheSameColumns` [Column "id", Column "name"]

-- |
checkConcatColumns :: Query -> Query
checkConcatColumns q = q <> Columns [Column "id", Column "name"]

