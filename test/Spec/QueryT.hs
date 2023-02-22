{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-missing-fields #-}

module Spec.QueryT
    ( queryTSpec
    ) where

import Prelude hiding (and, or, null, (&&), (||))
import Data.Text as T hiding (null)
import Test.Hspec
import Control.Monad
import Control.Monad.Identity
import System.IO.Unsafe

import QueryBuilder.Query

queryTSpec :: Spec
queryTSpec =
    describe "query transformer" $ do
      context "simple query" $ do
        it "using identity monad" $ do
          False `shouldNotBe` False

