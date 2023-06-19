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

import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Time
import Database.PostgreSQL.Simple.Types as PGT

import QueryBuilder
import QueryBuilder.PostgreSql as QB

queryQuery conn q = PG.query conn (PGT.Query $ structuredQuery q) (bindings q)
execQuery conn q = PG.execute conn (PGT.Query $ structuredQuery q) (bindings q)

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
    runSelectUsersJoinSpec connStr

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
                                     \(1, 'guest'        , null), \
                                     \(2, 'member'       , null), \
                                     \(3, 'prime member' , null), \
                                     \(4, 'staff'        , null), \
                                     \(5, 'admin'        , null), \
                                     \(6, 'developer'    , null), \
                                     \(7, 'tester'       , null)"
              , "INSERT INTO m_job_types (\"id\", \"name\", \"description\") OVERRIDING SYSTEM VALUE VALUES \
                                        \(1, 'start'     , null), \
                                        \(2, 'stop'      , null), \
                                        \(3, 'restart'   , null), \
                                        \(4, 'send'      , null), \
                                        \(5, 'resend'    , null), \
                                        \(6, 'receive'   , null), \
                                        \(7, 'verify'    , null), \
                                        \(8, 'clear'     , null), \
                                        \(9, 'review'    , null), \
                                        \(10, 'abort'    , null), \
                                        \(11, 'retry'    , null), \
                                        \(12, 'terminate', null)"
              , "INSERT INTO t_users (\"id\", \"level_id\", \"email\", \"registered\") OVERRIDING SYSTEM VALUE VALUES\
                                    \(1, 1, 'test_guest@mail.com'        , CURRENT_TIMESTAMP), \
                                    \(2, 2, 'test_member@mail.com'       , CURRENT_TIMESTAMP), \
                                    \(3, 3, 'test_prime member@mail.com' , CURRENT_TIMESTAMP), \
                                    \(4, 4, 'test_staff@mail.com'        , CURRENT_TIMESTAMP), \
                                    \(5, 5, 'test_admin@mail.com'        , CURRENT_TIMESTAMP), \
                                    \(6, 6, 'test_developer@mail.com'    , CURRENT_TIMESTAMP), \
                                    \(7, 7, 'test_tester@mail.com'       , CURRENT_TIMESTAMP)"
              , "INSERT INTO t_user_infos (\"id\", \"user_id\", \"name\", \"country\") OVERRIDING SYSTEM VALUE VALUES\
                                         \(1, 5, 'akane'   , 'japan'      ), \
                                         \(2, 6, 'miso'    , 'south korea'), \
                                         \(3, 7, 'shashee' , 'philippines'), \
                                         \(4, 4, 'son'     , 'thailand'   ), \
                                         \(5, 1, 'wanli'   , 'china'      ), \
                                         \(6, 2, 'ayumi'   , 'japan'      )"
              , "INSERT INTO t_jobs (\"id\", \"job_type_id\", \"user_id\", \"date\", \"successful\", \"retries\") OVERRIDING SYSTEM VALUE VALUES\
                                    \( 1,  1, 1, CURRENT_TIMESTAMP, true , null), \
                                    \( 2,  2, 1, CURRENT_TIMESTAMP, false,    5), \
                                    \( 3,  3, 2, CURRENT_TIMESTAMP, true , null), \
                                    \( 4,  4, 2, CURRENT_TIMESTAMP, true , null), \
                                    \( 5,  5, 2, CURRENT_TIMESTAMP, true , null), \
                                    \( 6,  6, 3, CURRENT_TIMESTAMP, false,   18), \
                                    \( 7,  7, 3, CURRENT_TIMESTAMP, true , null), \
                                    \( 8,  8, 4, CURRENT_TIMESTAMP, false,    2), \
                                    \( 9,  9, 4, CURRENT_TIMESTAMP, true , null), \
                                    \(10, 10, 4, CURRENT_TIMESTAMP, false,    3), \
                                    \(11, 11, 5, CURRENT_TIMESTAMP, true , null), \
                                    \(12, 12, 5, CURRENT_TIMESTAMP, true , null), \
                                    \(13,  1, 6, CURRENT_TIMESTAMP, false,   21), \
                                    \(14,  2, 6, CURRENT_TIMESTAMP, false,    0), \
                                    \(15,  3, 6, CURRENT_TIMESTAMP, true , null), \
                                    \(16,  4, 7, CURRENT_TIMESTAMP, true , null)"
              , "INSERT INTO t_tags (\"id\", \"name\") OVERRIDING SYSTEM VALUE VALUES\
                                    \(1, 'current'  ), \
                                    \(2, 'retry'    ), \
                                    \(3, 'check'    ), \
                                    \(4, 'invalid'  ), \
                                    \(5, 'important'), \
                                    \(6, 'replace'  ), \
                                    \(7, 'report'   ), \
                                    \(8, 'ignore'   )"
              , "INSERT INTO t_job_tags (\"id\", \"job_id\", \"tag_id\") OVERRIDING SYSTEM VALUE VALUES\
                                        \( 1,  1, 5), \
                                        \( 2,  2, 6), \
                                        \( 3,  3, 7), \
                                        \( 4,  4, 1), \
                                        \( 5,  5, 2), \
                                        \( 6,  1, 3), \
                                        \( 7,  2, 4), \
                                        \( 8,  1, 7), \
                                        \( 9,  2, 1), \
                                        \(10,  3, 2), \
                                        \(11,  7, 3), \
                                        \(12,  7, 1), \
                                        \(13,  8, 6), \
                                        \(14,  9, 7), \
                                        \(15, 10, 1), \
                                        \(16, 11, 2), \
                                        \(17,  9, 3), \
                                        \(18, 10, 4), \
                                        \(19,  1, 1), \
                                        \(20,  2, 2), \
                                        \(21, 12, 3), \
                                        \(22, 13, 4), \
                                        \(23, 11, 5), \
                                        \(24, 12, 6), \
                                        \(25, 13, 5), \
                                        \(26,  4, 6), \
                                        \(27, 15, 7), \
                                        \(28, 16, 4), \
                                        \(29, 15, 5), \
                                        \(30, 16, 2)"
              , "alter table t_users alter column id restart with 100"
              , "alter table t_user_infos alter column id restart with 100"
              , "alter table t_jobs alter column id restart with 100"
              , "alter table t_job_tags alter column id restart with 100"
              , "alter table t_tags alter column id restart with 100"
              , "alter table m_levels alter column id restart with 100"
              , "alter table m_job_types alter column id restart with 100"
              ]

runInsertUsersSpec :: String -> Spec
runInsertUsersSpec connStr =
  -- around (withConn connStr) $ do
  before (openConn connStr) $ do
    describe "postgresql insert users" $ do
      it "insert users" $ \conn -> do
        let q = buildInsertUsers
        structuredQuery q `shouldBe` "-- insert users: theurbanwanderess@nodetransit.com, ayumi@nodetransit.com, frostbane@nodetransit.com\n\
                                     \INSERT INTO t_users (level_id, email, registered) \
                                     \VALUES (?, ?, current_timestamp), \
                                            \(?, ?, current_timestamp), \
                                            \(?, ?, current_timestamp) \
                                     \RETURNING id"
        ids <- queryQuery conn q :: IO [Only Int64]
        -- _ <- dropUsers conn
        Prelude.length ids `shouldBe` 3
      where
        buildInsertUsers = createInsertUsers users
          where
            users = [ User { level_id = 1 , email = "theurbanwanderess@nodetransit.com" }
                    , User { level_id = 2 , email = "ayumi@nodetransit.com" }
                    , User { level_id = 5 , email = "frostbane@nodetransit.com" }
                    ]

runInsertUserInfosSpec :: String -> Spec
runInsertUserInfosSpec connStr =
  before (openConn connStr) $ do
    describe "postgresql insert infos" $ do
      it "insert user infos" $ \conn -> do
        let q = createInsertUserInfo UserInfo { user_id = 3, name = "nana" }
        structuredQuery q `shouldBe` "-- insert user info\n\
                                     \INSERT INTO t_user_infos (user_id, name) \
                                     \VALUES (?, ?) \
                                     \RETURNING id"
        ids <- queryQuery conn q :: IO [Only Int64]
        Prelude.length ids `shouldBe` 1

dropUsers conn = PG.execute_ conn "delete from t_users"

runSelectUsersJoinSpec :: String -> Spec
runSelectUsersJoinSpec connStr =
  before (openConn connStr) $ do
    describe "postgresql select users" $ do
      it "with single join" $ \conn -> do
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
        users <- queryQuery conn q :: IO [(Int, String, Maybe String, LocalTimestamp)]
        Prelude.length users `shouldBe` 3

runTransactionSpec ::


-- countUsers conn = PG.exe

