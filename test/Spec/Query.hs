{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-missing-fields #-}

module Spec.Query
    ( queryColumnSpec
    ) where

import Test.Hspec
import Spec.Util
import Prelude hiding (and, or, null, not)

import QueryBuilder.Query
import QueryBuilder.Condition
import QueryBuilder.Order

queryColumnSpec :: Spec
queryColumnSpec =
  describe "query semigroup/monoid" $ do
    context "query constructors should have empty columns" $ do
      it "select constructor" $ getColumnCount Select `shouldBe` 0
      it "insert constructor" $ getColumnCount Insert `shouldBe` 0
      it "update constructor" $ getColumnCount Update `shouldBe` 0
      it "delete constructor" $ getColumnCount Delete `shouldBe` 0

    context "building a full query in order should be valid" $ do
      let q = checkSelectQuery
      it "query type" $ query_type q `shouldBe` "SELECT"
      it "query table" $ query_table q `shouldBe` "users"
      it "query columns" $ query_columns q `isSameColumns` [Column "id", Column "name"]
      it "query conditions" $ (clause . query_conditions) q `shouldBe` "deleted <> ? OR deleted IS NOT NULL"
      it "query conditions" $ (bindings . query_conditions) q `shouldBe` [""]
      it "query order by" $ query_orderBy q `shouldBe` Asc
      it "query group by" $ query_groupBy q `shouldBe` [Column "type", Column "access"]

    context "building a full query in reversed order should be valid" $ do
      let q = checkSelectQueryNotInOrder
      it "query order by" $ query_groupBy q `shouldBe` [Column "genre"]
      it "query order by" $ query_orderBy q `shouldBe` Desc
      it "query conditions" $ (clause . query_conditions) q `shouldBe` "released <> ? AND released IS NOT NULL"
      it "query conditions" $ (bindings . query_conditions) q `shouldBe` [""]
      it "query columns" $ query_columns q `isSameColumns` [Column "id", Column "title"]
      it "query table" $ query_table q `shouldBe` "albums"
      it "query type" $ query_type q `shouldBe` "SELECT"


-- |
getColumnCount :: Query -> Int
getColumnCount = length . query_columns . (EmptyQuery <>)

-- |
checkSelectQuery :: Query
checkSelectQuery =
    Select
    <> From "users"
    <> Columns [Column "id", Column "name"]
    <> Where (runCondition $ do
        condition "deleted" (notEquals "")
        or "deleted" isNotNull
    )
    <> OrderBy Asc
    <> GroupBy [Column "type", Column "access" ]

checkSelectQueryNotInOrder :: Query
checkSelectQueryNotInOrder =
    GroupBy [Column "genre"]
    <> OrderBy Desc
    <> Where (runCondition $ do
        condition "released" (notEquals "")
        and "released" isNotNull
    )
    <> Columns [Column "id", Column "title"]
    <> From "albums"
    <> Select

