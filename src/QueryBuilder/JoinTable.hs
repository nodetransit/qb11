{-# LANGUAGE OverloadedStrings #-}

module QueryBuilder.JoinTable
    ( JoinTable(..)
    , JoinType(..)
    ) where

import Data.Text as T
import Data.Text (Text)
import Control.Monad

import QueryBuilder.Condition

data JoinTable = JoinTable
    { join_table      :: Text
    , join_type       :: JoinType
    , join_alias      :: Text
    , join_conditions :: QueryCondition
    }
    deriving ( Show
             , Eq
             )

data JoinType = Inner
              | Left
              | Right
              | Outer
              | Cross
              deriving ( Show
                       , Eq
                       )

