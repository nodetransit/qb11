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
import Data.Semigroup
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
import QueryBuilder.Set

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
        (condition_clause . query_conditions) q `shouldBe` "deleted IS NULL AND ( registered IS NOT NULL OR validated = ? ) AND blocked = ?"
        (condition_bindings . query_conditions) q `shouldBe` ["1", "0"]
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
        (condition_clause . query_conditions) q `shouldBe` "released = NOW()"
        (condition_bindings . query_conditions) q `shouldBe` mempty
      it "first join" $ do
        (join_type . head . query_joins) q `shouldBe` Inner
        (join_table . head . query_joins) q `shouldBe` "sales"
        (join_alias  . head . query_joins) q `shouldBe` Alias.None
        (condition_clause . join_conditions . head . query_joins) q `shouldBe` "( artists.id = sales.aid )"
        (condition_bindings . join_conditions . head . query_joins) q `shouldBe` mempty
      it "2nd join" $ do
        (join_type . head . tail . query_joins) q `shouldBe` Left
        (join_table . head . tail . query_joins) q `shouldBe` "infos"
        (join_alias  . head . tail . query_joins) q `shouldBe` Alias.None
        (condition_clause . join_conditions . head . tail . query_joins) q `shouldBe` "( artists.id = infos.aid AND infos.released = ? )"
        (condition_bindings . join_conditions . head . tail . query_joins) q `shouldBe` ["1"]

    context "table joins with alias" $ do
      let q = testQueryTransformerJoinAs
      it "query" $ do
        query_type q `shouldBe` "SELECT"
        (table_name . query_table) q `shouldBe` "customers"
        (table_alias . query_table) q `shouldBe` As "c"
        (length . query_joins) q `shouldBe` 1
        (condition_clause . query_conditions) q `shouldBe` mempty
        (condition_bindings . query_conditions) q `shouldBe` mempty
      it "query join" $ do
        (join_type . head . query_joins) q `shouldBe` Right
        (join_table . head . query_joins) q `shouldBe` "infos"
        (join_alias  . head . query_joins) q `shouldBe` As "ci"
        (condition_clause . join_conditions . head . query_joins) q `shouldBe` "( c.id = ci.customer_id AND c.valid = ? )"
        (condition_bindings . join_conditions . head . query_joins) q `shouldBe` ["1"]

    context "query group by" $ do
      let q = testQueryGroupBy
      it "query" $ do
        query_type q `shouldBe` "SELECT"
        (table_name . query_table) q `shouldBe` "users"
        (table_alias . query_table) q `shouldBe` Alias.None
        query_groupBy q `shouldBe` [Column "country"]
        (condition_clause . query_having) q `shouldBe` "count >= ?"
        (condition_bindings . query_having) q `shouldBe` ["5"]
        (order . query_orderBy) q `shouldBe` Asc
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
        (condition_clause . query_values) q `shouldBe` "(?, ?, ?, NOW()), (?, ?, ?, NOW()), (?, ?, ?, NOW())"
        (condition_bindings . query_values) q `shouldBe` ["mark", "us", "12th elm", "james", "ja", "blk. 1", "john", "en", "lot. 18"]
      prop "query columns" $ do
        query_columns q `shouldBeTheSameColumns` [Column "name", Column "country", Column "address", Column "register"]

    context "update values" $ do
      let q = testUpdateTable
      it "query" $ do
        query_type q `shouldBe` "UPDATE"
        (table_name . query_table) q `shouldBe` "customers"
        (condition_clause . query_conditions) q `shouldBe` "id IN (?, ?, ?)"
        (condition_bindings . query_conditions) q `shouldBe` ["1", "2", "3"]
        (set_clause . query_set) q `shouldBe` "name = ?, country = uk, address = ?"
        (set_bindings . query_set) q `shouldBe` ["ac", "1st st."]

    context "update values alternate" $ do
      let q = testUpdateTableAlt
      it "query" $ do
        query_type q `shouldBe` "UPDATE"
        (table_name . query_table) q `shouldBe` "users"
        (condition_clause . query_conditions) q `shouldBe` "id IN (?, ?)"
        (condition_bindings . query_conditions) q `shouldBe` ["1", "2"]
        (set_clause . query_set) q `shouldBe` "name = ?, deleted = NOW()"
        (set_bindings . query_set) q `shouldBe` ["ac"]

    context "delete values" $ do
      let q = testDelete
      it "query" $ do
        query_type q `shouldBe` "DELETE"
        (table_name . query_table) q `shouldBe` "users"
        (condition_clause . query_conditions) q `shouldBe` "unregistered = ? OR disabled IS NOT NULL"
        (condition_bindings . query_conditions) q `shouldBe` ["1"]

    context "maybe monad" $ do
      let q = testTransformWithMaybe
      it "query" $ do
        query_type q `shouldBe` "SELECT"
        (table_name . query_table) q `shouldBe` "users"
        (condition_clause . query_conditions) q `shouldBe` "registered = ?"
        (condition_bindings . query_conditions) q `shouldBe` ["1"]
        query_limit q `shouldBe` Just 18

    context "io monad" $ do
      let q = testTransformWithIO
      it "query" $ do
        query_type q `shouldBe` "SELECT"
        (table_name . query_table) q `shouldBe` "countries"
        (condition_clause . query_conditions) q `shouldBe` "name LIKE ?"
        (condition_bindings . query_conditions) q `shouldBe` ["%ice%"]
        query_limit q `shouldBe` Nothing
        (order . query_orderBy) q `shouldBe` Desc
      prop "query order by" $ do
        (order_columns . query_orderBy) q `shouldBeTheSameColumns` [Column "id"]
      prop "query columns" $ do
        query_columns q `shouldBeTheSameColumns` [Column "id", Column "name", ColumnAlias "area" (As "land_area")]

