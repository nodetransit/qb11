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
    context "build select" $ do
      let q = buildSelectUsers
      it "simple select query" $ do
        structuredQuery q `shouldBe` "-- test select query\n\
                           \-- test build order\n\
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

      let q = buildSelectUsersGroup
      it "select with grouping" $ do
        structuredQuery q `shouldBe` "-- test select query\n\
                           \SELECT DISTINCT COUNT(id) AS count, country\
                           \ FROM users\
                           \ WHERE deleted = ?\
                           \ GROUP BY country\
                           \ HAVING ( count >= ? )\
                           \ ORDER BY count DESC\
                           \ LIMIT 10"
        bindings q `shouldBe` [ "0"
                              , "3"
                              ]

      let q = buildSelectUsersWithBindings
      it "select bindings" $ do
        structuredQuery q `shouldBe` "-- test\n\
                           \-- select query bindings\n\
                           \SELECT\
                               \ COUNT(id) AS count,\
                               \ users.country,\
                               \ user_infos.address\
                           \ FROM users\
                           \ INNER JOIN user_infos\
                               \ ON ( user_infos.uid = users.id\
                                    \ AND user_infos.email <> ? )\
                           \ RIGHT JOIN transactions AS tx\
                               \ ON ( tx.uid = users.id\
                                    \ AND ( tx.failed = ?\
                                          \ OR tx.cancelled <> ?\
                                        \ )\
                                   \ )\
                           \ WHERE users.country LIKE ?\
                           \ GROUP BY users.country\
                           \ HAVING ( count > ? AND count <= ? )"
        bindings q `shouldBe` [ "''"
                              , "1"
                              , "''"
                              , "'%ice%'"
                              , "5"
                              , "10"
                              ]

    context "build update" $ do
      let q = buildUpdateCustomers
      it "simple update query" $ do
        structuredQuery q `shouldBe` "-- test build update query\n\
                           \-- with raw and parameterized values\n\
                           \UPDATE\
                           \ customers\
                           \ SET \
                               \name = ?, \
                               \country = ?, \
                               \address = ?, \
                               \updated = NOW()\
                           \ WHERE id IN (?, ?, ?)\
                           \ RETURNING *"
        bindings q `shouldBe` [ "ac"
                              , "uk"
                              , "1st st."
                              , "1"
                              , "2"
                              , "3"
                              ]

    context "build insert" $ do
      let q = buildInsertCustomers
      it "simple update query" $ do
        structuredQuery q `shouldBe` "-- test insert query\n\
                           \INSERT INTO\
                               \ customers\
                               \ (name, country, address, register, updated)\
                           \ VALUES\
                               \ (?, ?, ?, NOW(), null),\
                               \ (?, ?, ?, NOW(), null),\
                               \ (?, ?, ?, NOW(), null)\
                               \ RETURNING id, name AS inserted_name"
        bindings q `shouldBe` [ "mark"
                              , "us"
                              , "12th elm"
                              , "james"
                              , "ja"
                              , "blk. 1"
                              , "john"
                              , "en"
                              , "lot. 18"
                              ]

    context "build delete" $ do
      let q = buildDeleteUsers
      it "simple delete query" $ do
        structuredQuery q `shouldBe` "-- test delete query\n\
                           \DELETE FROM\
                               \ users\
                           \ WHERE unregistered = ?\
                                 \ OR disabled IS NOT NULL\
                            \ RETURNING id AS deleted_id"
        bindings q `shouldBe` ["1"]


