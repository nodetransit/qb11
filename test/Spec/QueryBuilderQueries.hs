{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-missing-fields #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Spec.QueryBuilderQueries
    ( buildSelectUsers
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
import QueryBuilder.PostgreSql

buildSelectUsers :: Query
buildSelectUsers = (runIdentity . runQueryT) $ do
    comment "test select query"
    select
    distinct
    from "users"
    columns [ "id"
            , column_ "COUNT(id)" (as "count")
            , column_ "CONCAT(firstname, ' ', lastname)" (as "full_name")
            , "country"
            , "address"
            ]
    where_ $ do
        condition "deleted" (equals false)
    orderBy [ "registered"
            , "age"
            ]
            asc
    groupBy [column "country"]
    having $ do
        condition "count" (gte "5")
    limit 18
    -- join
    -- join as

buildUpdate :: Query
buildUpdate =
    runQuery $ do
        update
        table "customers"
        set [ "name"    .= "ac"
            , "country" .= "uk"
            , "address" .= "1st st."
            , "updated" .= ("uk" :: Raw)
            ]
        where_ $ do
            condition "id" (isIn ["1", "2", "3"])

builddInsertCustomers :: Query
builddInsertCustomers =
    runQuery $ do
        insert
        into "customers"
        columns [ "name"
                , "country"
                , "address"
                , "register"
                , "updated"
                ]
        values [ [value "mark" , value "us", value "12th elm", value ("NOW()" :: Raw), value ("null" :: Raw)]
               , [value "james", value "ja", value "blk. 1"  , value ("NOW()" :: Raw), value ("null" :: Raw)]
               , [value "john" , value "en", value "lot. 18" , value ("NOW()" :: Raw), value ("null" :: Raw)]
               ]

buildDelete :: Query
buildDelete =
    runQuery $ do
        delete
        from "users"
        where_ $ do
            condition "unregistered" (equals true)
            or "disabled" (isNotNull)

