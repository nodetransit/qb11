{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -Wno-missing-fields #-}

module Spec.Query
    ( runColumnSpec
    ) where

import Test.Hspec
import Spec.Util
import QueryBuilder.Types

runColumnSpec :: Spec
runColumnSpec =
  describe "query semigroup/monoid" $ do
    context "query constructors should have empty columns" $ do
      it "select constructor" $ getColumnCount select `shouldBe` 0
      it "insert constructor" $ getColumnCount insert `shouldBe` 0
      it "update constructor" $ getColumnCount update `shouldBe` 0
      it "delete constructor" $ getColumnCount delete `shouldBe` 0
    context "building a full query should be valid" $ do
       it "select query" $ checkSelectQuery `shouldBe` True

-- |
getColumnCount :: Query -> Int
getColumnCount = length . query_columns

-- |
checkSelectQuery :: Bool
checkSelectQuery =  query_type q == "SELECT"
                 -- && query_table q == "users"
                 && query_columns q `isSameColumns` [Column "id", Column "name"]
  where
    q =  select
      -- <> from "users"
      <> columns [Column "id", Column "name"]

