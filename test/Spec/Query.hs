{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-missing-fields #-}

module Spec.Query
    ( queryColumnSpec
    ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Spec.Util
import Control.Monad
import Data.List hiding (and, or)
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
      it "query columns" $ query_columns q `shouldBeTheSameColumns` [Column "id", Column "name"]
      it "query conditions" $ (clause . query_conditions) q `shouldBe` "deleted <> ? OR deleted IS NOT NULL"
      it "query conditions" $ (bindings . query_conditions) q `shouldBe` [""]
      it "query order by" $ query_orderBy q `shouldBe` Asc
      it "query group by" $ query_groupBy q `shouldBe` [Column "type", Column "access"]

    context "building a full query in any order should be valid" $ do
      forM_ (permutations checkSelectQueryNotInOrder) $
        \queries -> do
           let q = foldl' (<>) defaultQuery queries
           it ("testing permutation :" ++ showQueries queries) $ do
               query_groupBy q `shouldBe` [Column "genre"]
               query_orderBy q `shouldBe` Desc
               (clause . query_conditions) q `shouldBe` "released <> ? AND released IS NOT NULL"
               (bindings . query_conditions) q `shouldBe` [""]
               query_table q `shouldBe` "albums"
               query_type q `shouldBe` "SELECT"
           prop ("testing permutation :" ++ showQueries queries) $ do
               query_columns q `shouldBeTheSameColumns` [Column "id", Column "title"]


-- |
getColumnCount :: Query -> Int
getColumnCount = length . query_columns . (EmptyQuery <>)

-- |
checkSelectQuery :: Query
checkSelectQuery =
    Select
    <> From "users"
    <> Columns [Column "id", Column "name"]
    <> Where (runConditionM $ do
        condition "deleted" (notEquals "")
        or "deleted" isNotNull
    )
    <> OrderBy Asc
    <> GroupBy [Column "type", Column "access" ]

checkSelectQueryNotInOrder :: [Query]
checkSelectQueryNotInOrder =
    [ GroupBy [Column "genre"]
    , OrderBy Desc
    , Where (runConditionM $ do
          condition "released" (notEquals "")
          and "released" isNotNull
      )
    , Columns [Column "id", Column "title"]
    , From "albums"
    , Select
    ]

