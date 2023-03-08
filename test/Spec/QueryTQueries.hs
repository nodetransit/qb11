{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-missing-fields #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Spec.QueryTQueries
    ( testQueryTransformer
    , testQueryTransformerJoin
    , testQueryTransformerJoinAs
    , testQueryGroupBy
    , testDistinctLimitEtc
    , testInsertValues
    , testUpdateTable
    , testUpdateTableAlt
    , testDelete
    , testTransformWithMaybe
    , testTransformWithIO
    ) where

import Prelude hiding (and, or, null, Left, Right)
import Data.Text as T hiding (null, length, head, tail, groupBy)
import Data.Semigroup
import Control.Monad.Identity hiding (join)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import System.IO.Unsafe

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

testInsertValues :: Query
testInsertValues =
    runQuery $ do
        insert
        into "customers"
        columns [ column "name"
                , column  "country"
                , column  "address"
                , column  "register"
                ]
        values [ [value "mark", value "us", value "12th elm", value ("NOW()" :: Raw)]
               , [value "james", value "ja", value "blk. 1", value ("NOW()" :: Raw)]
               , [value "john", value "en", value "lot. 18", value ("NOW()" :: Raw)]
               ]

testUpdateTable :: Query
testUpdateTable =
    runQuery $ do
        update
        table "customers"
        set [ "name"    .= "ac"
            , "country" .= ("uk" :: Raw)
            , "address" .= "1st st."
            ]
        where_ $ do
            condition "id" (isIn ["1", "2", "3"])

testUpdateTableAlt :: Query
testUpdateTableAlt =
    runQuery $ do
        update
        table "users"
        set [ setvalue "name" "ac"
            , setvalue "deleted" ("NOW()" :: Raw)
            ]
        where_ $ do
            condition "id" (isIn ["1", "2"])

testDelete :: Query
testDelete =
    runQuery $ do
        delete
        from "users"
        where_ $ do
            condition "unregistered" (equals true)
            or "disabled" (isNotNull)

testTransformWithMaybe :: Query
testTransformWithMaybe = (runMaybe . runQueryT) createQuery
  where
    runMaybe :: (Monoid a) => Maybe a -> a
    runMaybe (Just x) = x
    runMaybe _        = mempty

    createQuery :: QueryT Maybe
    createQuery = do
        select
        from "users"
        whereM_ $ do
            isDel <- lift getDeleted
            condition "registered" (equals true)
            ifM isDel $
                and "deleted" (equals true)
        lim <- lift getLimit
        ifM (lim > 0) $
           limit lim

    ifM b f = if b then f else return True

    getDeleted :: Maybe Bool
    getDeleted = Just False

    getLimit :: Maybe Int
    getLimit = Just 18

testTransformWithIO :: Query
testTransformWithIO = (unsafePerformIO . runQueryT) createQuery
  where
    createQuery :: QueryT IO
    createQuery = do
        select
        columns [ "id"
                , "name"
                , column_ "area" "land_area"
                ]
        from "countries"
        whereM_ $ do
            inputName <- lift $ wrapWildCard =<< getName
            condition "name" (like inputName)
        inputAsc <- lift getIsAscending
        if inputAsc
           then orderBy [ column "id"] asc
           else orderBy [ column "id"] desc

    wrapWildCard :: String -> IO (Text)
    wrapWildCard s = return $ T.pack $ "%" <> s <> "%"

    getName :: IO String
    getName = return "ice"

    getIsAscending :: IO Bool
    getIsAscending = return False

