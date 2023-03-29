{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-missing-fields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# Language DuplicateRecordFields #-}

module Spec.Engines.PostgreQueries
    ( User(..)
    , UserInfo(..)
    , Job(..)
    , createInsertUsers
    , createInsertUserInfo
    ) where

import Data.Text (Text)
import Data.Text as T hiding (null, length, head, tail, groupBy, map, filter, foldl)
import Data.Semigroup
import Data.Time

import QueryBuilder
import QueryBuilder.PostgreSql

data User = User
    { id         :: Int
    , level_id   :: Int
    , email      :: Text
    , registered :: LocalTime
    , deleted    :: Maybe LocalTime
    }

type Users = [User]

data UserInfo = UserInfo
    { id        :: Int
    , user_id   :: Int
    , name      :: Text
    , country   :: Maybe Text
    , address   :: Maybe Text
    , telephone :: Maybe Text
    }

data Job = Job
    { id            :: Int
    , job_type_id   :: Int
    , user_id       :: Int
    , date          :: LocalTime
    , successfull   :: Maybe Bool
    , retries       :: Maybe Int
    , cancelled     :: Maybe LocalTime
    , cancel_reason :: Maybe Text
    , failed        :: Maybe LocalTime
    , fail_reason   :: Maybe Text
    }

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
    returning "id"
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
    insert
    into "t_user_infos"
    columns [ "user_id"
            , "name"
            ]
    values [[ value $ (T.pack . show . (user_id :: UserInfo -> Int)) ui
            , value $ (name) ui
            ]]

