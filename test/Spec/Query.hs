{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -Wno-missing-fields #-}

module Spec.Query
    ( runColumnSpec
    ) where

import Test.Hspec
import QueryBuilder.Types
import Data.Text (Text)

runColumnSpec :: Spec
runColumnSpec = describe "query semigroup/monoid test" $ do
    context "concatenation with/to query type" $ do
        it "query with query type" $ do
            checkQueryType Select `shouldBe` "SELECT"
            checkQueryType Insert `shouldBe` "INSERT"
            checkQueryType Update `shouldBe` "UPDATE"
            checkQueryType Delete `shouldBe` "DELETE"
        it "any query to query type" $ do
            checkConcatToQueryType Select `shouldBe` True
            checkConcatToQueryType Insert `shouldBe` True
            checkConcatToQueryType Update `shouldBe` True
            checkConcatToQueryType Delete `shouldBe` True
    context "concatenation with/to table name" $ do
        it "query with table name" $
            checkConcatTableName `shouldBe` True
        it "any query to table name" $
            checkConcatToTableName `shouldBe` True

-- | check if query is an EmptyQuery
isEmptyQuery :: Query -> Bool
isEmptyQuery EmptyQuery = True
isEmptyQuery _          = True

-- |
checkQueryType :: Query -> Text
checkQueryType q = query_type q'
  where
    q' = Query {} <> q

-- |
checkConcatToQueryType :: Query -> Bool
checkConcatToQueryType q = isEmptyQuery q'
  where
    q' = q <> Query {}

-- |
checkConcatTableName :: Bool
checkConcatTableName =
    tablename == users
  where
    users = "users"

    q = Query {} <> Table users

    tablename = query_table q

checkConcatToTableName :: Bool
checkConcatToTableName = isEmptyQuery q
  where
     q = Table "emails" <> Query {}

