{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-missing-fields #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Spec.Query
    ( querySpec
    , permutationSpec
    ) where

import Test.Hspec
import Test.Hspec.Core.Spec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.HUnit.Lang
import Data.Semigroup
import Spec.Util
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Trans.State
import Data.List hiding (and, or)
import Prelude hiding (and, or, null, not, Left, Right)

import QueryBuilder.Internal.Query
import QueryBuilder.JoinTable
import QueryBuilder.Alias as Alias
import QueryBuilder.Condition
import QueryBuilder.QueryTable
import QueryBuilder.QueryOrder
import QueryBuilder.QueryOrder as Order
import QueryBuilder.Set
import QueryBuilder.Raw
import qualified QueryBuilder.Returning as Return

querySpec :: Spec
querySpec =
  describe "query semigroup/monoid" $ do
    context "query builder" $ do
      it ("semigroup append") $ (return . fst) =<< runStateT testSemigroup defaultQuery

    context "query constructors should have empty columns" $ do
      it "select constructor" $ getColumnCount Select `shouldBe` 0
      it "insert constructor" $ getColumnCount Insert `shouldBe` 0
      it "update constructor" $ getColumnCount Update `shouldBe` 0
      it "delete constructor" $ getColumnCount Delete `shouldBe` 0

    context "building a full query in order should be valid" $ do
      let q = checkSelectQuery
          j1 = (query_joins q)!!0
          j2 = (query_joins q)!!1
          j3 = (query_joins q)!!2
          j4 = (query_joins q)!!3
      it "query type" $ query_type q `shouldBe` "SELECT"
      it "query distinct" $ query_distinct q `shouldBe` True
      it "query table" $ (table_name . query_table) q `shouldBe` "users"
      it "query table" $ (table_alias . query_table) q `shouldBe` Alias.None
      it "query joins" $ (length . query_joins) q `shouldBe` 4
      it "query 1st join type" $ join_type j1 `shouldBe` Inner
      it "query 1st join table" $ join_table j1 `shouldBe` "infos"
      it "query 1st join alias" $ join_alias j1 `shouldBe` Alias.None
      it "query 1st join condition" $ (condition_clause . join_conditions) j1 `shouldBe` "( users.id = infos.uid AND users.disabled <> ? AND infos.deleted IS NOT NULL )"
      it "query 1st join condition" $ (condition_bindings . join_conditions) j1 `shouldBe` ["0"]
      it "query 2nd join type" $ join_type j2 `shouldBe` Right
      it "query 2nd join table" $ join_table j2 `shouldBe` "logs"
      it "query 2nd join alias" $ join_alias j2 `shouldBe` As "ul"
      it "query 2nd join condition" $ (condition_clause . join_conditions) j2 `shouldBe` "( users.id = ul.uid AND ul.type = ? )"
      it "query 2nd join condition" $ (condition_bindings . join_conditions) j2 `shouldBe` ["error"]
      it "query 3rd join type" $ join_type j3 `shouldBe` Left
      it "query 3rd join table" $ join_table j3 `shouldBe` "user_files"
      it "query 3rd join alias" $ join_alias j3 `shouldBe` Alias.None
      it "query 3rd join using" $ (condition_clause . join_using) j3 `shouldBe` "( id, date )"
      it "query 3rd join using" $ (condition_bindings . join_using) j3 `shouldBe` []
      it "query 4th join type" $ join_type j4 `shouldBe` Outer
      it "query 4th join table" $ join_table j4 `shouldBe` "audit"
      it "query 4th join alias" $ join_alias j4 `shouldBe` "user_audit"
      it "query 4th join using" $ (condition_clause . join_using) j4 `shouldBe` "( id, name )"
      it "query 4th join using" $ (condition_bindings . join_using) j4 `shouldBe` []
      it "query columns" $ query_columns q `shouldBeTheSameColumns` [Column "id", Column "name"]
      it "query conditions" $ (condition_clause . query_conditions) q `shouldBe` "deleted <> ? OR deleted IS NOT NULL"
      it "query conditions" $ (condition_bindings . query_conditions) q `shouldBe` [""]
      it "query order by" $ (order_columns . query_orderBy) q `shouldBeTheSameColumns` [Column "registered", Column "last_login"]
      it "query order by" $ (order . query_orderBy) q `shouldBe` Asc
      it "query group by" $ query_groupBy q `shouldBe` [Column "type", Column "access"]
      it "query having conditions" $ (condition_clause . query_having) q `shouldBe` "type LIKE ?"
      it "query having conditions" $ (condition_bindings . query_having) q `shouldBe` ["%admin%"]
      it "query limit" $ query_limit q `shouldBe` Just 12
      it "query offset" $ query_offset q `shouldBe` Just 3
      it "query comments" $ query_comments q `shouldBe` ["select user", "join with info and logs"]

testSemigroup :: StateT Query IO ()
testSemigroup = do
    q <- get
    lift $ do
        query_type        q `shouldBe` mempty
        query_table       q `shouldBe` QueryTable mempty Alias.None
        query_distinct    q `shouldBe` False
        query_columns     q `shouldBe` mempty
        query_values      q `shouldBe` mempty
        query_set         q `shouldBe` mempty
        query_groupBy     q `shouldBe` mempty
        query_having      q `shouldBe` mempty
        query_joins       q `shouldBe` mempty
        query_conditions  q `shouldBe` mempty
        query_orderBy     q `shouldBe` QueryOrder mempty Order.None
        query_limit       q `shouldBe` Nothing
        query_offset      q `shouldBe` Nothing
        query_returning   q `shouldBe` Return.Nothing
        query_comments    q `shouldBe` mempty

    put $ q <> Select
    q <- get
    lift $ do
        query_type        q `shouldBe` "SELECT"
        query_table       q `shouldBe` QueryTable mempty Alias.None
        query_distinct    q `shouldBe` False
        query_columns     q `shouldBe` mempty
        query_values      q `shouldBe` mempty
        query_set         q `shouldBe` mempty
        query_groupBy     q `shouldBe` mempty
        query_having      q `shouldBe` mempty
        query_joins       q `shouldBe` mempty
        query_conditions  q `shouldBe` mempty
        query_orderBy     q `shouldBe` QueryOrder mempty Order.None
        query_limit       q `shouldBe` Nothing
        query_offset      q `shouldBe` Nothing
        query_returning   q `shouldBe` Return.Nothing
        query_comments    q `shouldBe` mempty

    put $ q <> Distinct
    q <- get
    lift $ do
        query_type        q `shouldBe` "SELECT"
        query_table       q `shouldBe` QueryTable mempty Alias.None
        query_distinct    q `shouldBe` True
        query_columns     q `shouldBe` mempty
        query_values      q `shouldBe` mempty
        query_set         q `shouldBe` mempty
        query_groupBy     q `shouldBe` mempty
        query_having      q `shouldBe` mempty
        query_joins       q `shouldBe` mempty
        query_conditions  q `shouldBe` mempty
        query_orderBy     q `shouldBe` QueryOrder mempty Order.None
        query_limit       q `shouldBe` Nothing
        query_offset      q `shouldBe` Nothing
        query_returning   q `shouldBe` Return.Nothing
        query_comments    q `shouldBe` mempty

    put $ q <> Columns [Column "id", Column "name"]
    q <- get
    lift $ do
        query_type        q `shouldBe` "SELECT"
        query_table       q `shouldBe` QueryTable mempty Alias.None
        query_distinct    q `shouldBe` True
        (length . query_columns) q `shouldBe` 2
        query_values      q `shouldBe` mempty
        query_set         q `shouldBe` mempty
        query_groupBy     q `shouldBe` mempty
        query_having      q `shouldBe` mempty
        query_joins       q `shouldBe` mempty
        query_conditions  q `shouldBe` mempty
        query_orderBy     q `shouldBe` QueryOrder mempty Order.None
        query_limit       q `shouldBe` Nothing
        query_offset      q `shouldBe` Nothing
        query_returning   q `shouldBe` Return.Nothing
        query_comments    q `shouldBe` mempty

    put $ q <> Table "users"
    q <- get
    lift $ do
        query_type        q `shouldBe` "SELECT"
        query_table       q `shouldBe` QueryTable "users" Alias.None
        query_distinct    q `shouldBe` True
        (length . query_columns) q `shouldBe` 2
        query_values      q `shouldBe` mempty
        query_set         q `shouldBe` mempty
        query_groupBy     q `shouldBe` mempty
        query_having      q `shouldBe` mempty
        query_joins       q `shouldBe` mempty
        query_conditions  q `shouldBe` mempty
        query_orderBy     q `shouldBe` QueryOrder mempty Order.None
        query_limit       q `shouldBe` Nothing
        query_offset      q `shouldBe` Nothing
        query_returning   q `shouldBe` Return.Nothing
        query_comments    q `shouldBe` mempty

    put $ q <> Join Inner "infos" (runConditionM $ do
        condition "users.id" (equalsRaw "infos.uid")
        and "users.disabled" (notEquals false)
        and "infos.deleted" isNotNull
       )
    q <- get
    lift $ do
        query_type        q `shouldBe` "SELECT"
        query_table       q `shouldBe` QueryTable "users" Alias.None
        query_distinct    q `shouldBe` True
        (length . query_columns) q `shouldBe` 2
        query_values      q `shouldBe` mempty
        query_set         q `shouldBe` mempty
        query_groupBy     q `shouldBe` mempty
        query_having      q `shouldBe` mempty
        (length . query_joins) q `shouldBe` 1
        query_conditions  q `shouldBe` mempty
        query_orderBy     q `shouldBe` QueryOrder mempty Order.None
        query_limit       q `shouldBe` Nothing
        query_offset      q `shouldBe` Nothing
        query_returning   q `shouldBe` Return.Nothing
        query_comments    q `shouldBe` mempty

    put $ q <> JoinAlias Right "logs" (As "ul") (runConditionM $ do
        condition "users.id" (equalsRaw "ul.uid")
        and "ul.type" (equals "error")
       )
    q <- get
    lift $ do
        query_type        q `shouldBe` "SELECT"
        query_table       q `shouldBe` QueryTable "users" Alias.None
        query_distinct    q `shouldBe` True
        (length . query_columns) q `shouldBe` 2
        query_values      q `shouldBe` mempty
        query_set         q `shouldBe` mempty
        query_groupBy     q `shouldBe` mempty
        query_having      q `shouldBe` mempty
        (length . query_joins) q `shouldBe` 2
        query_conditions  q `shouldBe` mempty
        query_orderBy     q `shouldBe` QueryOrder mempty Order.None
        query_limit       q `shouldBe` Nothing
        query_offset      q `shouldBe` Nothing
        query_returning   q `shouldBe` Return.Nothing
        query_comments    q `shouldBe` mempty

    return ()

permutationSpec :: Spec
permutationSpec =
  describe "query builder order permutation" $ do
    context "building a full query in any order should be valid" $ do
      forM_ (permutations checkSelectQueryNotInOrder_pt1) $
        \queries -> do
           let q = foldl' (<>) defaultQuery queries
           it ("testing permutation: " ++ showQueries queries) $ do
               (order . query_orderBy) q `shouldBe` Desc
               (condition_clause . query_conditions) q `shouldBe` "released <> ? AND released IS NOT NULL"
               (condition_bindings . query_conditions) q `shouldBe` [""]
               (table_name . query_table) q `shouldBe` "albums"
               (table_alias . query_table) q `shouldBe` Alias.None
               query_type q `shouldBe` "SELECT"
               query_limit q `shouldBe` Nothing
               query_limit (q <> Limit 18) `shouldBe` Just 18
               query_offset q `shouldBe` Nothing
               query_offset (q <> Offset 12) `shouldBe` Just 12
               query_distinct q `shouldBe` False
               query_distinct (q <> Distinct) `shouldBe` True
               (show . query_joins) q `shouldBe` ""
               (length . query_joins) q `shouldBe` 2
               query_comments q `shouldBe` ["test comment"]
           prop ("testing permutation: " ++ showQueries queries) $ do
               query_columns q `shouldBeTheSameColumns` [Column "id", Column "title"]
           prop ("testing permutation :" ++ showQueries queries) $ do
               (order_columns . query_orderBy) q `shouldBeTheSameColumns` [Column "rating", Column "artist"]

    context "building a full query in any order should be valid" $ do
      forM_ (permutations checkSelectQueryNotInOrder_pt2) $
        \queries -> do
           let q = foldl' (<>) defaultQuery queries
           it ("testing permutation: " ++ showQueries queries) $ do
               query_distinct q `shouldBe` True
               query_groupBy q `shouldBe` [Column "genre"]
               (condition_clause . query_having) q `shouldBe` "genre LIKE ?"
               (condition_bindings . query_having) q `shouldBe` ["%prog%"]
               query_comments q `shouldBe` ["select ", "albums and ", "associated info", "filter unreleased ", "albums and ", "non-prog genres"]

    context "building an insert query in any order should be valid" $ do
      forM_ (permutations checkInsertQueryNotInOrder) $
        \queries -> do
           let q = foldl' (<>) defaultQuery queries
           it ("testing permutation: " ++ showQueries queries) $ do
               query_type q `shouldBe` "INSERT"
               (table_name . query_table) q `shouldBe` "users"
               (table_alias . query_table) q `shouldBe` Alias.None
               (condition_clause . query_values) q `shouldBe` "(?, akane), (2, ?), (?, ?)"
               (condition_bindings . query_values) q `shouldBe` ["1", "ayumi", "3", "ayami"]
           prop ("testing permutation: " ++ showQueries queries) $ do
               query_columns q `shouldBeTheSameColumns` [Column "id", Column "name"]

    context "building an update query in any order should be valid" $ do
      forM_ (permutations checkUpdateQueryNotInOrder) $
        \queries -> do
           let q = foldl' (<>) defaultQuery queries
           it ("testing permutation: " ++ showQueries queries) $ do
               query_type q `shouldBe` "UPDATE"
               (table_name . query_table) q `shouldBe` "users"
               (table_alias . query_table) q `shouldBe` Alias.None
               (condition_clause . query_conditions) q `shouldBe` "id <= ?"
               (condition_bindings . query_conditions) q `shouldBe` ["18"]
               (set_clause . query_set) q `shouldBe` "name = ?, value = ?, register = NOW()"
               (set_bindings . query_set) q `shouldBe` ["a", "v"]

-- |
getColumnCount :: Query -> Int
getColumnCount = length . query_columns . (EmptyQuery <>)

-- |
checkSelectQuery :: Query
checkSelectQuery =
    Select
    <> Distinct
    <> Columns [Column "id", Column "name"]
    <> Table "users"
    <> Join Inner "infos" (runConditionM $ do
        condition "users.id" (equalsRaw "infos.uid")
        and "users.disabled" (notEquals false)
        and "infos.deleted" isNotNull
       )
    <> JoinAlias Right "logs" (As "ul") (runConditionM $ do
        condition "users.id" (equalsRaw "ul.uid")
        and "ul.type" (equals "error")
       )
    <> JoinUsing Left "user_files" ["id", "date"]
    <> JoinAliasUsing Outer "audit" (As "user_audit") ["id", "name"]
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
    <> Offset 3
    <> Comment ["select user", "join with info and logs"]

-- | basic query permutations 1
checkSelectQueryNotInOrder_pt1 :: [Query]
checkSelectQueryNotInOrder_pt1 =
    [ Select
    , Columns [Column "id", Column "title"]
    , Table "albums"
    , Join Left "info" (runConditionM $ do
        condition "users.id" (equals "info.uid")
        and "deleted" isNotNull
      )
    , JoinUsing Inner "album_covers" ["id"]
    , Where (runConditionM $ do
          condition "released" (notEquals "")
          and "released" isNotNull
      )
    , OrderBy [Column "rating", Column "artist"] Desc
    , Comment ["test comment"]
    ]

-- | basic query permutations 2
--
--   split some of the permutations in another test to make the
--   test run time more reasonable
checkSelectQueryNotInOrder_pt2 :: [Query]
checkSelectQueryNotInOrder_pt2 =
    [ Select
    , Distinct
    , Columns [Column "id", Column "title"]
    , Table "albums"
    , GroupBy [Column "genre"]
    , Having (runConditionM $ do
        condition "genre" (like "%prog%")
    )
    , Comment ["select \nalbums and \n\rassociated info", "filter unreleased \ralbums and \r\nnon-prog genres"]
    ]

-- |
checkInsertQueryNotInOrder :: [Query]
checkInsertQueryNotInOrder =
    [ Insert
    , Table "users"
    , Columns [Column "id", Column "name"]
    , Values [ [Value "" "1" , Value "akane" ""]
             , [Value "2" ""  , Value "" "ayumi"]
             , [Value "" "3"  , Value "" "ayami"]
             ]
    ]

-- |
checkUpdateQueryNotInOrder :: [Query]
checkUpdateQueryNotInOrder =
    [ Update
    , Table "users"
    , Set [ SetValue "name" "" "a"
          , SetValue "value" "" "v"
          , SetValue "register" "NOW()" ""
          ]
    , Where (runConditionM $ do
          condition "id" (lte "18")
      )
    ]

