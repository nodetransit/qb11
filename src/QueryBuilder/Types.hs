{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module QueryBuilder.Types
    ( Column(..)
    , Operation(..)
    , Condition(..)
    , Query(..)
    , Join(..)
    ) where

import Data.Text as T
import Data.Text (Text)
import Data.Array as Array


data Column = Column         Text
            | ColumnAlias    Text Text
            | RawColumn      Text
            | RawColumnAlias Text Text

data Operation = Equals
               | NotEquals
               | Null
               | NotNull
               | Like
               | NotLike
             deriving Show

data Condition = Condition String Operation String
               | Group [Condition]
             deriving Show
-- data Condition = Condition Text | Array Condition

data Query = Query
    { query_table      :: Text
    , query_conditions :: Condition
    -- , query_bindings   :: [a]
    -- , query_orderBy    :: [a]
    -- , query_distinct   :: [b]
    -- , query_limit      :: [n]
    -- , query_joins      :: [a]
    }

data Join = Join
    { join_table      :: Text
    , join_alias      :: Text
    -- , join_bindings   :: Array Text
    , join_conditions :: Condition
    }

