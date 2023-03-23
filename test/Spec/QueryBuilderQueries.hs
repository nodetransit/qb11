{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-missing-fields #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Spec.QueryBuilderQueries
    ( buildSelectUsers
    , buildSelectUsersGroup
    , buildSelectUsersWithBindings
    , buildUpdateCustomers
    , buildInsertCustomers
    , buildDeleteUsers
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
    comment "test select query\n\ntest build order"
    select
    distinct
    from "users"
    columns [ "users.id"
            , column_ "COUNT(id)" (as "count")
            , column_ "CONCAT(firstname, ' ', lastname)" (as "full_name")
            , "users.country"
            , "user_infos.address"
            ]
    where_ $ do
        condition "deleted" (equals false)
    orderBy [ "users.registered"
            , "user_infos.age"
            ]
            asc
    groupBy [column "users.country"]
    having $ do
        condition "count" (gte "5")
    limit 12
    offset 18
    join "user_infos" on $ do
        condition "user_infos.uid" (equalsRaw "users.id")
        and "user_infos.email" isNotNull
    leftJoin_ "transactions" (as "tx") on $ do
        condition "tx.uid" (equalsRaw "users.id")
        and "tx.failed" (equals true)

buildSelectUsersGroup :: Query
buildSelectUsersGroup = (runIdentity . runQueryT) $ do
    comment "test select query"
    select
    distinct
    from "users"
    columns [ column_ "COUNT(id)" (as "count")
            , "country"
            ]
    where_ $ do
        condition "deleted" (equals false)
    orderBy [ "count" ]
            desc
    groupBy [column "country"]
    having $ do
        condition "count" (gte "3")
    limit 10

buildSelectUsersWithBindings :: Query
buildSelectUsersWithBindings = (runIdentity . runQueryT) $ do
    comment "test\n\nselect query bindings"
    select
    from "users"
    columns [ column_ "COUNT(id)" (as "count")
            , "users.country"
            , "user_infos.address"
            ]
    where_ $ do
        condition "users.country" (like "'%ice%'")
    groupBy [column "users.country"]
    having $ do
        condition "count" (gt "5")
        and "count" (lte "10")
    join "user_infos" on $ do
        condition "user_infos.uid" (equalsRaw "users.id")
        and "user_infos.email" (notEquals "''")
    rightJoin_ "transactions" (as "tx") on $ do
        condition "tx.uid" (equalsRaw "users.id")
        and_ $ do
            condition "tx.failed" (equals true)
            or "tx.cancelled" (notEquals "''")

buildUpdateCustomers :: Query
buildUpdateCustomers =
    runQuery $ do
        comments [ "test build update query"
                 , "with raw and parameterized values"
                 ]
        update
        table "customers"
        set [ "name"    .= "ac"
            , "country" .= "uk"
            , "address" .= "1st st."
            , "updated" .= ("NOW()" :: Raw)
            ]
        where_ $ do
            condition "id" (isIn ["1", "2", "3"])

buildInsertCustomers :: Query
buildInsertCustomers =
    runQuery $ do
        comment "test insert query"
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
        returning "id"

buildDeleteUsers :: Query
buildDeleteUsers =
    runQuery $ do
        comment "test delete query"
        delete
        from "users"
        where_ $ do
            condition "unregistered" (equals true)
            or "disabled" (isNotNull)
        returning "id"

