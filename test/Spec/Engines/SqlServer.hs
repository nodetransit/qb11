{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-missing-fields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# Language DuplicateRecordFields #-}

module Spec.Engines.SqlServer
    ( sqlServerSpec
    ) where

import System.Environment
import Prelude hiding (and, or, null, Left, Right)
import Data.Text as T hiding (null, length, head, tail, groupBy, map, filter, foldl, foldr)
import Data.Text.Encoding as T
import Data.Time
import Data.Semigroup
import Data.Int
import Control.Monad.Identity hiding (join)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Exception (bracket)
import System.IO.Unsafe
import Test.Hspec

import Spec.Util
import Spec.Engines.SqlServerQueries

import qualified Data.ByteString as BS

import QueryBuilder
import QueryBuilder.SqlServer as QB

import Fixtures.User
import Fixtures.UserInfo
import Fixtures.Job
import Fixtures.JobType

sqlServerSpec :: Spec
sqlServerSpec = do
    describe "sql server tests" $ do
        -- runTodo
        runInsertUsersSpec
        runSelectUsersJoinSpec
        runDeleteUsersSpec
        runUpdateUsersSpec

-- runTodo :: Spec
-- runTodo =
--     describe "unimplemented tests" $ do
--       it "x" $ shouldBeImplemented

runInsertUsersSpec :: Spec
runInsertUsersSpec =
    context "insert queries" $ do
      it "insert users" $ do
        let q = buildInsertUsers
        structuredQuery q `shouldBe` "-- insert users: theurbanwanderess@nodetransit.com, ayumi@nodetransit.com, frostbane@nodetransit.com\n\
                                     \INSERT INTO t_users (level_id, email, [registered]) \
                                     \OUTPUT Inserted.id AS inserted_id, Inserted.email AS inserted_email \
                                     \VALUES (?, ?, current_timestamp), \
                                            \(?, ?, current_timestamp), \
                                            \(?, ?, current_timestamp)"
  where
    buildInsertUsers = createInsertUsers users
      where
        users = [ User { level_id = 1 , email = "theurbanwanderess@nodetransit.com" }
                , User { level_id = 2 , email = "ayumi@nodetransit.com" }
                , User { level_id = 5 , email = "frostbane@nodetransit.com" }
                ]

runSelectUsersJoinSpec :: Spec
runSelectUsersJoinSpec =
    context "select queries" $ do
      it "with single join" $ do
        let q = createSelectUserWithInfo 3
        structuredQuery q `shouldBe` "-- select users with user info\n\
                                     \SELECT\
                                         \ TOP (3)\
                                         \ t_users.id AS user_id\
                                        \, t_users.email\
                                        \, t_user_infos.[name]\
                                        \, t_users.[registered] \
                                     \FROM t_users \
                                     \LEFT JOIN t_user_infos \
                                         \ON ( t_users.id = t_user_infos.user_id ) \
                                     \ORDER BY user_id ASC"

runDeleteUsersSpec :: Spec
runDeleteUsersSpec =
    context "delete queries" $ do
      it "delete users" $ do
        let q = createDeleteUserWithEmailLike "%test.com"
        structuredQuery q `shouldBe` "DELETE FROM t_users \
                                      \OUTPUT Deleted.id AS [delete], Deleted.email AS email \
                                      \WHERE email LIKE ?"

runUpdateUsersSpec :: Spec
runUpdateUsersSpec  =
    context "update queries" $ do
      it "without limit" $ do
        let q = createUpdateUserName "茜"
        structuredQuery q `shouldBe` "UPDATE t_user_infos \
                                      \SET [name] = ? \
                                      \OUTPUT Inserted.id \
                                      \WHERE user_id = ?"
      it "with limit" $ do
        let q = createClearUserPhones 100
        structuredQuery q `shouldBe` "UPDATE TOP (100) \
                                      \t_user_infos \
                                      \SET telephone = NULL"

