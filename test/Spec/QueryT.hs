{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-missing-fields #-}

module Spec.QueryT
    ( queryTSpec
    ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Prelude hiding (and, or, null, (&&), (||))
import Data.Text as T hiding (null)
import Control.Monad
import Control.Monad.Identity
import System.IO.Unsafe

import Spec.Util

import QueryBuilder.Query
import QueryBuilder.Column

queryTSpec :: Spec
queryTSpec =
  describe "query transformer" $ do
    context "simple query" $ do

      let q = testQueryTransformer
      it "using identity monad" $ do
        query_type q `shouldBe` "SELECT"
        query_table q `shouldBe` "users"
      prop "using identity monad" $ do
        query_columns q `shouldBeTheSameColumns` [Column "id", Column "name", Column "level"]

testQueryTransformer :: Query
testQueryTransformer = (runIdentity . runQueryT) createQuery
  where
    createQuery :: QueryT Identity
    createQuery = do
        select
        from "users"
        columns [Column "id", Column "name", Column "level"]

