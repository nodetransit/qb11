{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -Wno-missing-fields #-}

module Spec.Query
    ( runColumnSpec
    ) where

import Test.Hspec
import Spec.Util
import QueryBuilder.Query

runColumnSpec :: Spec
runColumnSpec =
  describe "query semigroup/monoid" $ do
    context "query constructors should have empty columns" $ do
      it "select constructor" $ getColumnCount Select `shouldBe` 0
      it "insert constructor" $ getColumnCount Insert `shouldBe` 0
      it "update constructor" $ getColumnCount Update `shouldBe` 0
      it "delete constructor" $ getColumnCount Delete `shouldBe` 0
    context "building a full query should be valid" $ do
       it "select query in order" $ checkSelectQuery `shouldBe` True
       it "select query not in order" $ checkSelectQueryNotInOrder `shouldBe` True

-- |
getColumnCount :: Query -> Int
getColumnCount = length . query_columns . (EmptyQuery <>)

-- |
checkSelectQuery :: Bool
checkSelectQuery =  query_type q == "SELECT"
                 && query_table q == "users"
                 && query_columns q `isSameColumns` [Column "id", Column "name"]
  where
    q =  Select
      <> From "users"
      <> Columns [Column "id", Column "name"]

checkSelectQueryNotInOrder :: Bool
checkSelectQueryNotInOrder =  query_type q == "SELECT"
                           && query_table q == "users"
                           && query_columns q `isSameColumns` [Column "id", Column "name"]
  where
    q =  Columns [Column "id", Column "name"]
      <> From "users"
      <> Select

