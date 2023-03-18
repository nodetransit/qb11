{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-missing-fields #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Spec.Engines.PostgreQueries
    ( 
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

createInsertUser :: [User] -> Query
createInsertUser users = runQuery $ do
    comment $ "insert users: " <> userEmails
  where
    userEmails = T.intercalate ", " $ map email users

