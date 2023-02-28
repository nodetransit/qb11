{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-missing-fields #-}

module Spec.QueryT
    ( queryTSpec
    ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (on)

import Prelude hiding (and, or, null, Left, Right)
import Data.Text as T hiding (null, length, head, tail, groupBy)
import Control.Monad hiding (join)
import Control.Monad.Identity hiding (join)
import System.IO.Unsafe

import Spec.Util
import Spec.QueryTQueries

import QueryBuilder.Query
import QueryBuilder.Alias as Alias
import QueryBuilder.Column
import QueryBuilder.QueryTable
import QueryBuilder.Condition
import QueryBuilder.QueryOrder
import QueryBuilder.JoinTable

queryTSpec :: Spec
queryTSpec =
  describe "query transformer" $ do

    context "simple query" $ do
      let q = testQueryTransformer
      it "using identity monad" $ do
        query_distinct q `shouldBe` False
        query_type q `shouldBe` "SELECT"
        (table_name . query_table) q `shouldBe` "users"
        (table_alias . query_table) q `shouldBe` Alias.None
        (order . query_orderBy) q `shouldBe` Desc
        (clause . query_conditions) q `shouldBe` "deleted IS NULL AND ( registered IS NOT NULL OR validated = ? ) AND blocked = ?"
        (bindings . query_conditions) q `shouldBe` ["1", "0"]
        query_limit q `shouldBe` Nothing
      prop "using identity monad" $ do
        (order_columns . query_orderBy) q `shouldBeTheSameColumns` [Column "registered_on", Column "last_login"]
      prop "using identity monad" $ do
        query_columns q `shouldBeTheSameColumns` [Column "id", Column "name", Column "level"]

    context "table joins" $ do
      let q = testQueryTransformerJoin
      it "using identity monad" $ do
        query_type q `shouldBe` "SELECT"
        (table_name . query_table) q `shouldBe` "artists"
        (table_alias . query_table) q `shouldBe` Alias.None
        (length . query_joins) q `shouldBe` 2
        (clause . query_conditions) q `shouldBe` "released = NOW()"
        (bindings . query_conditions) q `shouldBe` mempty
      it "first join" $ do
        (join_type . head . query_joins) q `shouldBe` Inner
        (join_table . head . query_joins) q `shouldBe` "sales"
        (join_alias  . head . query_joins) q `shouldBe` Alias.None
        (clause . join_conditions . head . query_joins) q `shouldBe` "( artists.id = sales.aid )"
        (bindings . join_conditions . head . query_joins) q `shouldBe` mempty
      it "2nd join" $ do
        (join_type . head . tail . query_joins) q `shouldBe` Left
        (join_table . head . tail . query_joins) q `shouldBe` "infos"
        (join_alias  . head . tail . query_joins) q `shouldBe` Alias.None
        (clause . join_conditions . head . tail . query_joins) q `shouldBe` "( artists.id = infos.aid AND infos.released = ? )"
        (bindings . join_conditions . head . tail . query_joins) q `shouldBe` ["1"]

    context "table joins with alias" $ do
      let q = testQueryTransformerJoinAs
      it "query" $ do
        query_type q `shouldBe` "SELECT"
        (table_name . query_table) q `shouldBe` "customers"
        (table_alias . query_table) q `shouldBe` As "c"
        (length . query_joins) q `shouldBe` 1
        (clause . query_conditions) q `shouldBe` mempty
        (bindings . query_conditions) q `shouldBe` mempty
      it "query join" $ do
        (join_type . head . query_joins) q `shouldBe` Right
        (join_table . head . query_joins) q `shouldBe` "infos"
        (join_alias  . head . query_joins) q `shouldBe` As "ci"
        (clause . join_conditions . head . query_joins) q `shouldBe` "( c.id = ci.customer_id AND c.valid = ? )"
        (bindings . join_conditions . head . query_joins) q `shouldBe` ["1"]

    context "query group by" $ do
      let q = testQueryGroupBy
      it "query" $ do
        query_type q `shouldBe` "SELECT"
        (table_name . query_table) q `shouldBe` "users"
        (table_alias . query_table) q `shouldBe` Alias.None
        query_groupBy q `shouldBe` [Column "country"]
        (clause . query_having) q `shouldBe` "count >= ?"
        (bindings . query_having) q `shouldBe` ["5"]
      prop "query columns" $ do
        query_columns q `shouldBeTheSameColumns` [ColumnAlias "COUNT(id)" (As "count"), Column "country"]
      prop "query order by" $ do
        (order_columns . query_orderBy) q `shouldBeTheSameColumns` [Column "count"]

    context "query distinct, limit, comment, etc" $ do
      let q = testDistinctLimitEtc
      it "query" $ do
        query_distinct q `shouldBe` True
        query_type q `shouldBe` "SELECT"
        (table_name . query_table) q `shouldBe` "customers"
        (table_alias . query_table) q `shouldBe` Alias.None
        query_groupBy q `shouldBe` [Column "country"]
        query_limit q `shouldBe` Just 18
      prop "query columns" $ do
        query_columns q `shouldBeTheSameColumns` [ColumnAlias "COUNT(id)" (As "count"), Column "country"]

    context "insert values" $ do
      let q = testInsertValues
      it "query" $ do
        query_type q `shouldBe` "INSERT"
        (table_name . query_table) q `shouldBe` "customers"
        (table_alias . query_table) q `shouldBe` Alias.None
        (clause . query_values) q `shouldBe` "(?, ?, ?), (?, ?, ?), (?, ?, ?)"
        (bindings . query_values) q `shouldBe` ["mark", "us", "12th elm", "james", "ja", "blk. 1", "john", "en", "lot. 18"]
      prop "query columns" $ do
        query_columns q `shouldBeTheSameColumns` [Column "name", Column "country", Column "address"]

    context "update values" $ do
      it "implement" $ do
        True `shouldBe` False

    context "delete values" $ do
      it "implement" $ do
        True `shouldBe` False

    context "maybe monad" $ do
      it "implement" $ do
        True `shouldBe` False

    context "io monad" $ do
      it "implement" $ do
        True `shouldBe` False

