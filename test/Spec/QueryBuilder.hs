{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-missing-fields #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Spec.QueryBuilder
    ( queryBuilderSpec
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
import Spec.QueryBuilderQueries

import QueryBuilder
import QueryBuilder.PostgreSql

queryBuilderSpec :: Spec
queryBuilderSpec = 
  describe "query and bindings" $ do
    context "create full select" $ do
      let q = buildSelectUsers
      it "query" $ do
        query q `shouldBe` "-- test select query\n\
                           \-- test build order\n\
                           \SELECT\
                               \ users.id,\
                               \ COUNT(id) AS count,\
                               \ CONCAT(firstname, ' ', lastname) AS full_name,\
                               \ users.country,\
                               \ user_infos.address\
                           \ FROM users\
                           \ INNER JOIN user_infos\
                               \ ON ( user_infos.uid = users.id AND user_infos.email IS NOT NULL )\
                           \ LEFT JOIN transactions AS tx\
                               \ ON ( tx.uid = users.id AND tx.failed = ? )\
                           \ WHERE deleted = ?\
                           \ GROUP BY users.country\
                           \ HAVING ( count >= ? )\
                           \ ORDER BY\
                               \ users.registered,\
                               \ user_infos.age\
                               \ ASC\
                           \ LIMIT 18"
        bindings q `shouldBe` [ "1"
                              , "0"
                              , "5"
                              ]

    context "create select with grouping" $ do
      let q = buildSelectUsersGroup
      it "query" $ do
        query q `shouldBe` "-- test select query\n\
                           \SELECT COUNT(id) AS count, country\
                           \ FROM users\
                           \ WHERE deleted = ?\
                           \ GROUP BY country\
                           \ HAVING ( count >= ? )\
                           \ ORDER BY count DESC\
                           \ LIMIT 10"
        bindings q `shouldBe` [ "0"
                              , "3"
                              ]

