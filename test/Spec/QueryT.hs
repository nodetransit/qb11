{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-missing-fields #-}

module Spec.QueryT
    ( queryTSpec
    ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (on)

import Prelude hiding (and, or, null, Left)
import Data.Text as T hiding (null, length, head, tail)
import Control.Monad hiding (join)
import Control.Monad.Identity hiding (join)
import System.IO.Unsafe

import Spec.Util

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
        query_type q `shouldBe` "SELECT"
        (table_name . query_table) q `shouldBe` "users"
        (table_alias . query_table) q `shouldBe` Alias.None
        (order . query_orderBy) q `shouldBe` Desc
        (clause . query_conditions) q `shouldBe` "deleted IS NULL AND ( registered IS NOT NULL OR validated = ? ) AND blocked = ?"
        (bindings . query_conditions) q `shouldBe` ["1", "0"]
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
        (clause . join_conditions . head . query_joins) q `shouldBe` "artists.id = sales.aid"
        (bindings . join_conditions . head . query_joins) q `shouldBe` mempty
      it "2nd join" $ do
        (join_type . head . tail . query_joins) q `shouldBe` Left
        (join_table . head . tail . query_joins) q `shouldBe` "infos"
        (join_alias  . head . tail . query_joins) q `shouldBe` Alias.None
        (clause . join_conditions . head . tail . query_joins) q `shouldBe` "artists.id = infos.aid AND infos.released = ?"
        (bindings . join_conditions . head . tail . query_joins) q `shouldBe` ["1"]

testQueryTransformer :: Query
testQueryTransformer = (runIdentity . runQueryT) createQuery
  where
    createQuery :: QueryT Identity
    createQuery = do
        select
        from "users"
        columns [Column "id", Column "name", Column "level"]
        where_ $ do
            condition "deleted" isNull
            and_ $ do
                condition "registered" isNotNull
                or "validated" (equals true)
            and "blocked" (equals false)
        orderBy [Column "registered_on", Column "last_login"] Desc

testQueryTransformerJoin :: Query
testQueryTransformerJoin = (runIdentity . runQueryT) createQuery
  where
    createQuery :: QueryT Identity
    createQuery = do
        select
        from "artists"
        columns [Column "artists.id", Column "sales.year", Column "infos.album"]
        join "sales" on $ do
            condition "artists.id" (equalsRaw "sales.aid")
        leftJoin "infos" on $ do
            condition "artists.id" (equalsRaw "infos.aid")
            and "infos.released" (equals true)
        where_ $ do
            condition "released" (equalsRaw "NOW()")

