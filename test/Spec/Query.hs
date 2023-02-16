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
import QueryBuilder.QueryOrder

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
      it "query distinct" $ query_distinct q `shouldBe` True
      it "query table" $ query_table q `shouldBe` "users"
      it "query columns" $ query_columns q `shouldBeTheSameColumns` [Column "id", Column "name"]
      it "query conditions" $ (clause . query_conditions) q `shouldBe` "deleted <> ? OR deleted IS NOT NULL"
      it "query conditions" $ (bindings . query_conditions) q `shouldBe` [""]
      it "query order by" $ (columns . query_orderBy) q `shouldBeTheSameColumns` [Column "registered", Column "last_login"]
      it "query order by" $ (order . query_orderBy) q `shouldBe` Asc
      it "query group by" $ query_groupBy q `shouldBe` [Column "type", Column "access"]
      it "query having conditions" $ (clause . query_having) q `shouldBe` "type LIKE ?"
      it "query having conditions" $ (bindings . query_having) q `shouldBe` ["%admin%"]
      it "query limit" $ query_limit q `shouldBe` Just 12

    context "building a full query in any order should be valid" $ do
      forM_ (permutations checkSelectQueryNotInOrderNotDistinct) $
        \queries -> do
           let q = foldl' (<>) defaultQuery queries
           it ("testing permutation: " ++ showQueries queries) $ do
               query_groupBy q `shouldBe` [Column "genre"]
               (order . query_orderBy) q `shouldBe` Desc
               (clause . query_conditions) q `shouldBe` "released <> ? AND released IS NOT NULL"
               (bindings . query_conditions) q `shouldBe` [""]
               query_table q `shouldBe` "albums"
               query_type q `shouldBe` "SELECT"
               (clause . query_having) q `shouldBe` "genre LIKE ?"
               (bindings . query_having) q `shouldBe` ["%prog%"]
               query_limit q `shouldBe` Nothing
               query_limit (q <> Limit 18) `shouldBe` Just 18
               query_distinct q `shouldBe` False
               query_distinct (q <> Distinct) `shouldBe` True
           prop ("testing permutation: " ++ showQueries queries) $ do
               query_columns q `shouldBeTheSameColumns` [Column "id", Column "title"]
           prop ("testing permutation :" ++ showQueries queries) $ do
               (columns . query_orderBy) q `shouldBeTheSameColumns` [Column "rating", Column "artist"]

    context "building an insert query in any order should be valid" $ do
      forM_ (permutations checkInsertQueryNotInOrder) $
        \queries -> do
           let q = foldl' (<>) defaultQuery queries
           it ("testing permutation: " ++ showQueries queries) $ do
               query_type q `shouldBe` "INSERT"
               query_table q `shouldBe` "users"
               (clause . query_values) q `shouldBe` "(?, ?), (?, ?), (?, ?)"
               (bindings . query_values) q `shouldBe` ["1", "akane", "2", "ayumi", "3", "ayami"]
           prop ("testing permutation: " ++ showQueries queries) $ do
               query_columns q `shouldBeTheSameColumns` [Column "id", Column "name"]

-- |
getColumnCount :: Query -> Int
getColumnCount = length . query_columns . (EmptyQuery <>)

-- |
checkSelectQuery :: Query
checkSelectQuery =
    Select
    <> Distinct
    <> Columns [Column "id", Column "name"]
    <> From "users"
    <> Where (runConditionM $ do
        condition "deleted" (notEquals "")
        or "deleted" isNotNull
    )
    <> GroupBy [Column "type", Column "access" ]
    <> Having (runConditionM $ do
        condition "type" (like "%admin%")
    )
    <> OrderBy [Column "registered", Column "last_login"] Asc
    <> Limit 12

checkSelectQueryNotInOrderNotDistinct :: [Query]
checkSelectQueryNotInOrderNotDistinct =
    [ Select
    , Columns [Column "id", Column "title"]
    , From "albums"
    , Where (runConditionM $ do
          condition "released" (notEquals "")
          and "released" isNotNull
      )
    , GroupBy [Column "genre"]
    , Having (runConditionM $ do
        condition "genre" (like "%prog%")
    )
    , OrderBy [Column "rating", Column "artist"] Desc
    ]

-- | tests run too long so I 'chose' not to test the order Distinct was concatenated
checkSelectQueryNotInOrderDistinct :: [Query]
checkSelectQueryNotInOrderDistinct =
    [ Select
    , Distinct
    , Columns [Column "id", Column "title"]
    , From "albums"
    , Where (runConditionM $ do
          condition "released" (notEquals "")
          and "released" isNotNull
      )
    , GroupBy [Column "genre"]
    , Having (runConditionM $ do
        condition "genre" (like "%prog%")
    )
    , OrderBy [Column "rating", Column "artist"] Desc
    ]

-- |
checkInsertQueryNotInOrder :: [Query]
checkInsertQueryNotInOrder =
    [ Insert
    , Into "users"
    , Columns [Column "id", Column "name"]
    , Values [["1", "akane"], ["2", "ayumi"], ["3", "ayami"]]
    ]

