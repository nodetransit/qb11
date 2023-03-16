{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -Wno-missing-fields #-}

module Spec.Column
    ( columnSpec
    ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Semigroup
import Spec.Util

import QueryBuilder.Internal.Query
import QueryBuilder.Alias

columnSpec :: Spec
columnSpec =
  describe "column semigroup/monoid" $ do
    context "concatenation with columns" $ do
      prop "should overwrite columns" $ do
        (query_columns . checkConcatColumns) Select `shouldBeTheSameColumns` [Column "id", Column "name"]
    context "string overloade" $ do
      prop "should overloaded columns" $ do
        query_columns checkToString `shouldBeTheSameColumns` [Column "id", Column "name", ColumnAlias "value" "val"]

-- |
checkConcatColumns :: Query -> Query
checkConcatColumns q = q <> Columns [Column "id", Column "name"]

-- |
checkToString :: Query
checkToString = do
    defaultQuery <> Columns ["id", "name", ColumnAlias "value" (As "val")]

