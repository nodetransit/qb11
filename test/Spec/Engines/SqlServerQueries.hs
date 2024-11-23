{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-missing-fields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DuplicateRecordFields
           , OverloadedRecordDot 
           , DisambiguateRecordFields
#-}

module Spec.Engines.SqlServerQueries
    ( User(..)
    , UserInfo(..)
    , JobType(..)
    , Job(..)
    , createInsertUsers
    , createInsertUserInfo
    , createSelectUserWithInfo
    , createDeleteUserWithEmailLike
    , createCountUsersWithEmail
    , createUpdateUserName
    , createClearUserPhones
    , createUserJob
    , createTag
    ) where

import Prelude hiding (id)
import Data.Text (Text)
import Data.Text as T hiding (null, length, head, tail, groupBy, map, filter, foldl)
import Data.Semigroup
import Data.Time

import QueryBuilder
import QueryBuilder.SqlServer

{- HLINT ignore "Redundant $" -}

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

data JobType = JobType
    { id          :: Int
    , name        :: Maybe Text
    , description :: Maybe Text
    }

createInsertUsers :: Users -> Query
createInsertUsers users = runQuery $ do
    comment $ "insert users: " <> userEmails
    insert
    into "t_users"
    columns [ "level_id"
            , "email"
            , "[registered]"
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
    values [[ value $ T.pack . show $ ui.user_id
            , value $ ui.name
            ]]

createSelectUserWithInfo :: Int -> Query
createSelectUserWithInfo n = runQuery $ do
    comment $ "select users with user info"
    select
    columns [ column_ "t_users.id" (as "user_id")
            , "t_users.email"
            , "t_user_infos.[name]"
            , "t_users.[registered]"
            ]
    from "t_users"
    leftJoin "t_user_infos" on $ do
        condition "t_users.id" (eqRaw "t_user_infos.user_id")
    orderBy ["user_id"]
            asc
    limit n

createDeleteUserWithEmailLike :: String -> Query
createDeleteUserWithEmailLike email = runQuery $ do
    delete
    from "t_users"
    where_ $ do
        condition "email" (like $ T.pack email)
    returning [column_ "id" (as "[delete]"), column_ "email" (as "email")]

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
    values [ [ value $ T.pack . show $ job.id
             , value $ T.pack . show $ user.id
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

createUpdateUserName :: Text -> Query
createUpdateUserName name = runQuery $ do
    update
    table "t_user_infos"
    set ["[name]" .= name]
    where_ $ condition "user_id" (equals "1001")
    returning ["id"]

createClearUserPhones :: Int -> Query
createClearUserPhones max = runQuery $ do
    update
    table "t_user_infos"
    set ["telephone" .= ("NULL" :: Raw)]
    limit max
