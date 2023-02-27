{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-missing-fields #-}

module Spec.QueryTQueries
    ( testQueryTransformer
    , testQueryTransformerJoin
    , testQueryTransformerJoinAs
    , testQueryGroupBy
    , testDistinctLimitEtc
    ) where

import Prelude hiding (and, or, null, Left, Right)
-- import Data.Text as T hiding (null, length, head, tail, groupBy)
-- import Control.Monad hiding (join)
import Control.Monad.Identity hiding (join)
-- import System.IO.Unsafe

import Spec.Util

import QueryBuilder

testQueryTransformer :: Query
testQueryTransformer = (runIdentity . runQueryT) createQuery
  where
    createQuery :: QueryT Identity
    createQuery = do
        select
        from "users"
        columns [ column "id"
                , column "name"
                , column "level"]
        where_ $ do
            condition "deleted" isNull
            and_ $ do
                condition "registered" isNotNull
                or "validated" (equals true)
            and "blocked" (equals false)
        orderBy [ column "registered_on"
                , column "last_login"
                ] desc

testQueryTransformerJoin :: Query
testQueryTransformerJoin = (runIdentity . runQueryT) createQuery
  where
    createQuery :: QueryT Identity
    createQuery = do
        select
        from "artists"
        columns [ column "artists.id"
                , column "sales.year"
                , column "infos.album"
                ]
        join "sales" on $ do
            condition "artists.id" (equalsRaw "sales.aid")
        leftJoin "infos" on $ do
            condition "artists.id" (equalsRaw "infos.aid")
            and "infos.released" (equals true)
        where_ $ do
            condition "released" (equalsRaw "NOW()")

testQueryTransformerJoinAs :: Query
testQueryTransformerJoinAs =
    runQuery $ do
        select
        from_ "customers" (as "c")
        columns [ column "c.id"
                , column "c.name"
                , column "ci.address"]
        rightJoin_ "infos" (as "ci") on $ do
            condition "c.id" (equalsRaw "ci.customer_id")
            and "c.valid" (equals true)

testQueryGroupBy :: Query
testQueryGroupBy =
    runQuery $ do
        select
        from "users"
        columns [ column_ "COUNT(id)" (as "count")
                , column  "country"
                ]
        groupBy [column "country"]
        having $ do
            condition "count" (gte "5")
        orderBy [column "count"] asc

testDistinctLimitEtc :: Query
testDistinctLimitEtc =
    runQuery $ do
        comment "test query using distinct"
        select
        distinct
        columns [ column_ "COUNT(id)" (as "count")
                , column  "country"
                ]
        from "customers"
        groupBy [column "country"]
        limit 18

