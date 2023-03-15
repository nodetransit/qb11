{-# LANGUAGE OverloadedStrings #-}

module QueryBuilder.JoinTable
    ( JoinTable(..)
    , JoinType(..)
    ) where

import Data.Text as T
import Data.Text (Text)
import Control.Monad

import Prelude hiding (Left, Right)
import QueryBuilder.Alias
import QueryBuilder.Condition

data JoinTable = JoinTable
    { join_table      :: Text
    , join_type       :: JoinType
    , join_alias      :: Alias
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
              deriving ( Eq
                       )

instance Show (JoinType) where
    show (Inner) = "INNER"
    show (Left)  = "LEFT"
    show (Right) = "RIGHT"
    show (Outer) = "OUTER"
    show (Cross) = "CROSS"

