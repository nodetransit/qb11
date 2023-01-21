{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -Wno-missing-fields #-}

module Spec.Table
    ( runTableSpec
    ) where

import Test.Hspec
import QueryBuilder.Types

runTableSpec :: Spec
runTableSpec =
  describe "table semigroup/monoid" $ do
    context "concatenation with table" $ do
      it "should overwrite select table" $ checkConcatSelectTable `shouldBe` True
      it "should overwrite insert table" $ checkConcatInsertTable `shouldBe` True
      it "should overwrite update table" $ checkConcatUpdateTable `shouldBe` True
      it "should overwrite delete table" $ checkConcatDeleteTable `shouldBe` True

-- |
checkConcatSelectTable :: Bool
checkConcatSelectTable = query_table q' == "users"
  where
    q' = select <> from "users"

-- |
checkConcatInsertTable :: Bool
checkConcatInsertTable = query_table q' == "emails"
  where
    q' = insert <> into "emails"

-- |
checkConcatUpdateTable :: Bool
checkConcatUpdateTable = query_table q' == "infos"
  where
    q' = update <> table "infos"

-- |
checkConcatDeleteTable :: Bool
checkConcatDeleteTable = query_table q' == "accounts"
  where
    q' = delete <> from "accounts"
