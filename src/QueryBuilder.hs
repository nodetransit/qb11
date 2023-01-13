{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module QueryBuilder
    ( Query (..)
    , Join (..)
    , Column (..)
    , createQuery
    ) where

import Data.Text as T
import Data.Text (Text)
import Data.Array as Array


data Column = Column Text
            | Column Text Text
            | RawColumn Text

data Condition = Condition Text | Array Condition

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

createQuery :: Text -> Query
createQuery tableName = Query { query_table      = tableName
                              -- , query_conditions = []
                              }

