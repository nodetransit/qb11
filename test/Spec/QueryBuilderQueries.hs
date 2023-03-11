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

buildSelectUsers :: Query
buildSelectUsers = (runIdentity . runQueryT) $ do
    select
    from "users"
    columns [ "id"
            , column_ "CONCAT(firstname, ' ', lastname)" (as "full_name")
            , "address"
            ]
    where_ $ do
        condition "deleted" (equals false)
    orderBy [ "registered"
            , "age"
            ] asc

