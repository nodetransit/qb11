{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -Wno-missing-fields #-}

module Spec.Table
    ( tableSpec
    ) where

import Test.Hspec
import QueryBuilder.Internal.Query

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
checkConcatSelectTable = query_table q' == "users"
  where
    q' = Select <> Table "users"

-- |
checkConcatInsertTable :: Bool
checkConcatInsertTable = query_table q' == "emails"
  where
    q' = Insert <> Table "emails"

-- |
checkConcatUpdateTable :: Bool
checkConcatUpdateTable = query_table q' == "infos"
  where
    q' = Update <> Table "infos"

-- |
checkConcatDeleteTable :: Bool
checkConcatDeleteTable = query_table q' == "accounts"
  where
    q' = Delete <> Table "accounts"
