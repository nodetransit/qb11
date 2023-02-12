{-# LANGUAGE OverloadedStrings #-}

module QueryBuilder.Join
    ( Join(..)
    ) where

import Data.Text as T
import Data.Text (Text)

import QueryBuilder.Condition

data Join = Join
    { join_table      :: Text
    , join_alias      :: Text
    , join_conditions :: QueryCondition
    }


