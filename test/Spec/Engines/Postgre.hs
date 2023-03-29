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

import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Types as PGT

import QueryBuilder
import QueryBuilder.PostgreSql as QB

-- | open a connection
--
-- @example
-- ```
-- openConn "postgresql://testdata:a7a2E@testdata-persistence:5432/qb11"
-- ```
openConn = connectPostgreSQL . T.encodeUtf8 . T.pack
withConn connStr action =
    bracket
        (openConn connStr)
        action
        close

postgreSpec :: Spec
postgreSpec = do
    connStr <- runIO $ lookupEnv "_TEST_PGDB_CONN_STR"
    case connStr of
        Just c -> runPostgreSpec c
        _      -> return ()

runPostgreSpec :: String -> Spec
runPostgreSpec connStr = do
    runIO $ initializeData connStr
    runInsertUsersSpec connStr
    runInsertUserInfosSpec connStr

initializeData :: String -> IO ()
initializeData connStr = do
    conn <- openConn connStr
    mapM_ (PG.execute_ conn) queries
    close conn
  where
    queries = [ "truncate t_users cascade"
              , "truncate t_tags cascade"
              , "truncate m_levels cascade"
              , "truncate m_job_types cascade"
              , "INSERT INTO m_levels (\"id\", \"name\", \"description\") OVERRIDING SYSTEM VALUE VALUES\
                                     \(1, 'guest', null), \
                                     \(2, 'member', null), \
                                     \(3, 'prime member', null), \
                                     \(4, 'staff', null), \
                                     \(5, 'admin', null), \
                                     \(6, 'developer', null), \
                                     \(7, 'tester', null);"
              , "INSERT INTO m_job_types (\"id\", \"name\", \"description\") OVERRIDING SYSTEM VALUE VALUES \
                                        \(1, 'start', null), \
                                        \(2, 'stop', null), \
                                        \(3, 'restart', null), \
                                        \(4, 'send', null), \
                                        \(5, 'resend', null), \
                                        \(6, 'receive', null), \
                                        \(7, 'verify', null), \
                                        \(8, 'clear', null), \
                                        \(9, 'review', null), \
                                        \(10, 'abort', null), \
                                        \(11, 'retry', null), \
                                        \(12, 'terminate', null);"
              , "INSERT INTO t_users (\"id\", \"level_id\", \"email\", \"registered\") OVERRIDING SYSTEM VALUE VALUES\
                                    \(1, 1, 'test_guest@mail.com', CURRENT_TIMESTAMP), \
                                    \(2, 2, 'test_member@mail.com', CURRENT_TIMESTAMP), \
                                    \(3, 3, 'test_prime member@mail.com', CURRENT_TIMESTAMP), \
                                    \(4, 4, 'test_staff@mail.com', CURRENT_TIMESTAMP), \
                                    \(5, 5, 'test_admin@mail.com', CURRENT_TIMESTAMP), \
                                    \(6, 6, 'test_developer@mail.com', CURRENT_TIMESTAMP), \
                                    \(7, 7, 'test_tester@mail.com', CURRENT_TIMESTAMP);"
              , "INSERT INTO t_jobs (\"id\", \"job_type_id\", \"user_id\", \"date\") OVERRIDING SYSTEM VALUE VALUES\
                                    \( 1,  1, 1, CURRENT_TIMESTAMP)\
                                    \( 2,  2, 1, CURRENT_TIMESTAMP)\
                                    \( 3,  3, 2, CURRENT_TIMESTAMP)\
                                    \( 4,  4, 2, CURRENT_TIMESTAMP)\
                                    \( 5,  5, 2, CURRENT_TIMESTAMP)\
                                    \( 6,  6, 3, CURRENT_TIMESTAMP)\
                                    \( 7,  7, 3, CURRENT_TIMESTAMP)\
                                    \( 8,  8, 4, CURRENT_TIMESTAMP)\
                                    \( 9,  9, 4, CURRENT_TIMESTAMP)\
                                    \(10, 10, 4, CURRENT_TIMESTAMP)\
                                    \(11, 11, 5, CURRENT_TIMESTAMP)\
                                    \(12, 12, 5, CURRENT_TIMESTAMP)\
                                    \(13,  1, 6, CURRENT_TIMESTAMP)\
                                    \(14,  2, 6, CURRENT_TIMESTAMP)\
                                    \(15,  3, 6, CURRENT_TIMESTAMP)\
                                    \(16,  4, 7, CURRENT_TIMESTAMP);"
              , "INSERT INTO t_tags (\"id\", \"name\") OVERRIDING SYSTEM VALUE VALUES\
                                    \(1, 'current')\
                                    \(2, 'retry')\
                                    \(3, 'check')\
                                    \(4, 'invalid')\
                                    \(5, 'important')\
                                    \(6, 'consideration')\
                                    \(7, 'report')"
              , "INSERT INTO t_job_tags (\"id\", \"job_id\", \"tag_id\") OVERRIDING SYSTEM VALUE VALUES\
                                        \( 1,  1, 5)\
                                        \( 2,  2, 6)\
                                        \( 3,  3, 7)\
                                        \( 4,  4, 1)\
                                        \( 5,  5, 2)\
                                        \( 6,  1, 3)\
                                        \( 7,  2, 4)\
                                        \( 8,  1, 7)\
                                        \( 9,  2, 1)\
                                        \(10,  3, 2)\
                                        \(11,  6, 3)\
                                        \(12,  7, 1)\
                                        \(13,  8, 6)\
                                        \(14,  9, 7)\
                                        \(15, 10, 1)\
                                        \(16, 11, 2)\
                                        \(17,  9, 3)\
                                        \(18, 10, 4)\
                                        \(19,  1, 1)\
                                        \(20,  2, 2)\
                                        \(21, 12, 3)\
                                        \(22, 13, 4)\
                                        \(23, 11, 5)\
                                        \(24, 12, 6)\
                                        \(25, 13, 5)\
                                        \(26, 14, 6)\
                                        \(27, 15, 7)\
                                        \(28, 16, 4)\
                                        \(29, 15, 5)\
                                        \(30, 16, 2)"
              ]

runInsertUsersSpec :: String -> Spec
runInsertUsersSpec connStr =
  -- around (withConn connStr) $ do
  before (openConn connStr) $ do
    describe "postgresql insert users" $ do
      it "insert users" $ \conn -> do
        let q = buildInsertUsers
        structuredQuery q `shouldBe` "-- insert users: guest@mail.com, member@mail.com, admin@mail.com\n\
                                     \INSERT INTO t_users (level_id, email, registered) \
                                     \VALUES (?, ?, current_timestamp), \
                                            \(?, ?, current_timestamp), \
                                            \(?, ?, current_timestamp) \
                                     \RETURNING id"
        ids <- execQuery conn q
        -- _ <- dropUsers conn
        Prelude.length ids `shouldBe` 3

runInsertUserInfosSpec :: String -> Spec
runInsertUserInfosSpec connStr =
  before (openConn connStr) $ do
    describe "postgresql insert infos" $ do
      it "insert user infos" $ \conn -> do
        let q = createInsertUserInfo UserInfo { user_id = 1, name = "" }
        structuredQuery q `shouldBe` "-- insert user infos\n\
                                     \INSERT INTO t_user_infos (level_id, email, registered) \
                                     \VALUES (?, ?, current_timestamp), \
                                            \(?, ?, current_timestamp), \
                                            \(?, ?, current_timestamp) \
                                     \RETURNING id"

execQuery conn q = PG.query conn (PGT.Query $ structuredQuery q) (bindings q) :: IO [Only Int64]

buildInsertUsers = createInsertUsers users
  where
    users = [ User { level_id = 1 , email = "guest@mail.com" }
            , User { level_id = 2 , email = "member@mail.com" }
            , User { level_id = 5 , email = "admin@mail.com" }
            ]

dropUsers conn = PG.execute_ conn "delete from t_users"

-- countUsers conn = PG.exe
