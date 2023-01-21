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
      it "select constructor" $ getColumnCount select_ `shouldBe` 0
      it "insert constructor" $ getColumnCount insert_ `shouldBe` 0
      it "update constructor" $ getColumnCount update_ `shouldBe` 0
      it "delete constructor" $ getColumnCount delete_ `shouldBe` 0
    context "building a full query should be valid" $ do
       it "select query in order" $ checkSelectQuery `shouldBe` True
       it "select query not in order" $ checkSelectQueryNotInOrder `shouldBe` True

-- |
getColumnCount :: Query -> Int
getColumnCount = length . query_columns

-- |
checkSelectQuery :: Bool
checkSelectQuery =  query_type q == "SELECT"
                 && query_table q == "users"
                 && query_columns q `isSameColumns` [Column "id", Column "name"]
  where
    q =  select_
      <> from_ "users"
      <> columns_ [Column "id", Column "name"]

checkSelectQueryNotInOrder :: Bool
checkSelectQueryNotInOrder =  query_type q == "SELECT"
                           && query_table q == "users"
                           && query_columns q `isSameColumns` [Column "id", Column "name"]
  where
    q =  columns_ [Column "id", Column "name"]
      <> from_ "users"
      <> select_

