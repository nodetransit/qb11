{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-missing-fields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Spec.Engines.PostgreQueries
    ( createInsertUsers
    , createInsertUserInfo
    , createSelectUserWithInfo
    , createDeleteUserWithEmail
    , createCountUsersWithEmail
    , createUserJob
    , createTag
    ) where

import Prelude hiding (id)
import Data.Text (Text)
import Data.Text as T hiding (null, length, head, tail, groupBy, map, filter, foldl)
import Data.Semigroup
import Data.Time

import QueryBuilder
import QueryBuilder.PostgreSql

import Fixtures.User
import Fixtures.UserInfo
import Fixtures.Job
import Fixtures.JobType

{- HLINT ignore "Redundant $" -}

createInsertUsers :: Users -> Query
createInsertUsers users = runQuery $ do
    comment $ "insert users: " <> userEmails
    insert
    into "t_users"
    columns [ "level_id"
            , "email"
            , "registered"
            ]
    values userValues
    returning [column_ "id" (as "inserted_id"), column_ "email" (as "inserted_email")]
  where
    userEmails = T.intercalate ", " $ map email users
    userValues = map toInsertValues users
    toInsertValues u = [ value $ (T.pack . show . level_id) u
                       , value $ (email) u
                       , value ("current_timestamp" :: Raw)
                       ]

createInsertUserInfo :: UserInfo -> Query
createInsertUserInfo ui = runQuery $ do
    comment $ "insert user info"
    returning_ "id"
    insert
    into "t_user_infos"
    columns [ "user_id"
            , "name"
            ]
    values [[ value $ (T.pack . show . (user_id :: UserInfo -> Int)) ui
            , value $ (name :: UserInfo -> Text) ui
            ]]

createSelectUserWithInfo :: Int -> Query
createSelectUserWithInfo n = runQuery $ do
    comment $ "select users with user info"
    select
    columns [ column_ "t_users.id" (as "user_id")
            , "t_users.email"
            , "t_user_infos.name"
            , "t_users.registered"
            ]
    from "t_users"
    leftJoin "t_user_infos" on $ do
        condition "t_users.id" (eqRaw "t_user_infos.user_id")
    orderBy ["user_id"]
            asc
    limit n

createDeleteUserWithEmail :: String -> Query
createDeleteUserWithEmail email = runQuery $ do
    delete
    from "t_users"
    where_ $ do
        condition "email" (equals $ T.pack email)

createCountUsersWithEmail :: [String] -> Query
createCountUsersWithEmail emails = runQuery $ do
    select
    columns [ column_ "COUNT(*)" (as "count")
            ]
    from "t_users"
    where_ $ do
        condition "email" (isIn $ map T.pack emails)

createUserJob :: User -> JobType -> Query
createUserJob user job = runQuery $ do
    comment $ "insert a job for a user"
    insert
    into "t_jobs"
    columns [ column "job_type_id"
            , column "user_id"
            , column "date"
            ]
    values [ [ value $ (T.pack . show . (id :: JobType -> Int)) job
             , value $ (T.pack . show . (id :: User -> Int)) user
             , value ("CURRENT_TIMESTAMP" :: Raw)
             ] ]

createTag :: Int -> Text -> Query
createTag id name = runQuery $ do
    comment $ "insert job tag '" <> name <> "'"
    insert
    into "t_tags"
    columns [ column "id"
            , column "name"
            ]
    values [ [ value $ (T.pack . show) id
             , value $ name
             ] ]


