{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -Wno-missing-fields #-}

module Spec.Table
    ( tableSpec
    ) where

import Test.Hspec
import Data.Semigroup
import QueryBuilder.Internal.Query
import QueryBuilder.QueryTable

tableSpec :: Spec
tableSpec =
  describe "table semigroup/monoid" $ do
    context "concatenation with table" $ do
      it "should overwrite select table" $ checkConcatSelectTable `shouldBe` True
      it "should overwrite insert table" $ checkConcatInsertTable `shouldBe` True
      it "should overwrite update table" $ checkConcatUpdateTable `shouldBe` True
      it "should overwrite delete table" $ checkConcatDeleteTable `shouldBe` True

-- |
checkConcatSelectTable :: Bool
checkConcatSelectTable = (table_name . query_table) q' == "users"
  where
    q' = Select <> Table "users"

-- |
checkConcatInsertTable :: Bool
checkConcatInsertTable = (table_name . query_table) q' == "emails"
  where
    q' = Insert <> Table "emails"

-- |
checkConcatUpdateTable :: Bool
checkConcatUpdateTable = (table_name . query_table) q' == "infos"
  where
    q' = Update <> Table "infos"

-- |
checkConcatDeleteTable :: Bool
checkConcatDeleteTable = (table_name . query_table) q' == "accounts"
  where
    q' = Delete <> Table "accounts"
