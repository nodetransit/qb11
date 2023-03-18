{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-missing-fields #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Spec.Engines.Postgre
    ( postgreSpec
    ) where

import Prelude hiding (and, or, null, Left, Right)
import Data.Text as T hiding (null, length, head, tail, groupBy)
import Data.Semigroup
import Control.Monad.Identity hiding (join)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import System.IO.Unsafe
import Test.Hspec

import Spec.Util
import Spec.Engines.PostgreQueries

import QueryBuilder
import QueryBuilder.PostgreSql

postgreSpec :: Spec
postgreSpec =
  describe "postgresql insert" $ do
    it "not implemented" $ do
      shouldBeImplemented
