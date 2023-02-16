{-# LANGUAGE OverloadedStrings #-}

module QueryBuilder.JoinTable
    ( JoinTable(..)
    ) where

import Data.Text as T
import Data.Text (Text)
import Control.Monad

import QueryBuilder.Condition

data JoinTable = JoinTable
    { join_table      :: Text
    , join_type       :: Text
    , join_alias      :: Text
    , join_conditions :: QueryCondition
    }

instance Monoid (JoinTable) where
    (<>) (JoinTable jL) (JoinTable jR) =

