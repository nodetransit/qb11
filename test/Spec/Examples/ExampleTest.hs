{-# LANGUAGE OverloadedStrings #-}

module Spec.Examples.ExampleTest
    ( selectExampleSpec
    ) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Data.Text as T hiding (null, length, head, tail, groupBy)

import QueryBuilder
import QueryBuilder.PostgreSql

import Spec.Examples.SelectExample

selectExampleSpec :: Spec
selectExampleSpec =
  describe "query and bindings" $ do
    context "build select" $ do
      let q = buildSelectExample
      it "simple select query" $ do
        let sql = query q :: Text
        sql `shouldBe` "-- test select query\n\
                       \-- example\n\
                       \SELECT DISTINCT\
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
                       \ LIMIT 12\
                       \ OFFSET 18"
        bindings q `shouldBe` [ "1"
                              , "0"
                              , "5"
                              ]
