{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-missing-fields #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Spec.Engines.Postgre
    ( postgreSpec
    ) where

import Prelude hiding (and, or, null, Left, Right)
import Data.Text as T hiding (null, length, head, tail, groupBy)
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

openConn = connectPostgreSQL "postgresql://testdata:a7a2E@testdata-persistence:5432/qb11"
withConn action =
    bracket
        openConn
        action
        close

postgreSpec :: Spec
postgreSpec =
  before openConn $ do
  -- around withConn $ do
    describe "postgresql insert" $ do
      it "insert users" $ \conn -> do
        let q = buildInsertUsers
        structuredQuery q `shouldBe` "-- insert users: guest@mail.com, member@mail.com, admin@mail.com\n\
                                     \INSERT INTO t_users (level_id, email, registered) \
                                     \VALUES (?, ?, current_timestamp), \
                                            \(?, ?, current_timestamp), \
                                            \(?, ?, current_timestamp) \
                                     \RETURNING id"
        ids <- execQuery conn q
        _ <- dropUsers conn
        Prelude.length ids `shouldBe` 3

execQuery conn q = PG.query conn (PGT.Query $ structuredQuery q) (bindings q) :: IO [Only Int64]

buildInsertUsers = createInsertUsers users
  where
    users = [ User { level_id = 1 , email = "guest@mail.com" }
            , User { level_id = 2 , email = "member@mail.com" }
            , User { level_id = 5 , email = "admin@mail.com" }
            ]

dropUsers conn = PG.execute_ conn "delete from t_users"

-- countUsers conn = PG.exe
