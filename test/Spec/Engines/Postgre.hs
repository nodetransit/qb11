{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-missing-fields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# Language DuplicateRecordFields #-}

module Spec.Engines.Postgre
    ( postgreSpec
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
import Spec.Engines.PostgreQueries

import qualified Data.ByteString as BS

import QueryBuilder
import QueryBuilder.PostgreSql as QB

import Fixtures.User
import Fixtures.UserInfo
import Fixtures.Job
import Fixtures.JobType

postgreSpec :: Spec
postgreSpec = do
    describe "postgresql tests" $ do
        runInsertUsersSpec
        runInsertUserInfosSpec
        runSelectUsersJoinSpec

-- runTrue :: Spec
-- runTrue =
--     describe "a" $ do
--       it "c" $ do
--         True `shouldBe` True

runInsertUsersSpec :: Spec
runInsertUsersSpec =
    context "insert queries" $ do
      it "insert users" $ do
        let q = buildInsertUsers
        structuredQuery q `shouldBe` "-- insert users: theurbanwanderess@nodetransit.com, ayumi@nodetransit.com, frostbane@nodetransit.com\n\
                                     \INSERT INTO t_users (level_id, email, registered) \
                                     \VALUES (?, ?, current_timestamp), \
                                            \(?, ?, current_timestamp), \
                                            \(?, ?, current_timestamp) \
                                     \RETURNING id AS inserted_id, email AS inserted_email"
  where
    buildInsertUsers = createInsertUsers users
      where
        users = [ User { level_id = 1 , email = "theurbanwanderess@nodetransit.com" }
                , User { level_id = 2 , email = "ayumi@nodetransit.com" }
                , User { level_id = 5 , email = "frostbane@nodetransit.com" }
                ]

runInsertUserInfosSpec :: Spec
runInsertUserInfosSpec =
   context "insert queries" $ do
     it "insert user infos" $ \conn -> do
       let q = createInsertUserInfo UserInfo { user_id = 3, name = "nana" }
       structuredQuery q `shouldBe` "-- insert user info\n\
                                    \INSERT INTO t_user_infos (user_id, name) \
                                    \VALUES (?, ?) \
                                    \RETURNING id"

runSelectUsersJoinSpec :: Spec
runSelectUsersJoinSpec =
    context "select queries" $ do
      it "with single join" $ do
        let q = createSelectUserWithInfo 3
        structuredQuery q `shouldBe` "-- select users with user info\n\
                                     \SELECT\
                                         \ t_users.id AS user_id\
                                        \, t_users.email\
                                        \, t_user_infos.name\
                                        \, t_users.registered \
                                     \FROM t_users \
                                     \LEFT JOIN t_user_infos \
                                         \ON ( t_users.id = t_user_infos.user_id ) \
                                     \ORDER BY user_id ASC \
                                     \LIMIT 3"

