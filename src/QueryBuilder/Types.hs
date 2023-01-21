{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module QueryBuilder.Types
    ( Query(..)
    , Operation(..)
    , Condition(..)
    , Join(..)
    ) where

import Data.Text as T
import Data.Text (Text)
import Data.Array as Array
import Control.Monad
import Control.Applicative


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

data Query = EmptyQuery
           | Select
           | Insert
           | Update
           | Delete
           | Table Text
           | Columns [Text]
           | Query { query_type       :: Text
                   , query_table      :: Text
                   , query_columns    :: [Column]
                   -- , query_conditions :: Condition
                   -- , query_bindings   :: [a]
                   -- , query_orderBy    :: [a]
                   -- , query_distinct   :: [b]
                   -- , query_limit      :: [n]
                   -- , query_joins      :: [a]
                   }

modify_query :: Query -> Query -> Query
modify_query q@(Query {}) Select    = q { query_type = "SELECT" }
modify_query q@(Query {}) Insert    = q { query_type = "INSERT" }
modify_query q@(Query {}) Update    = q { query_type = "UPDATE" }
modify_query q@(Query {}) Delete    = q { query_type = "DELETE" }
modify_query q@(Query {}) (Table t) = q { query_table = t }
modify_query q@(Query {}) _         = q
modify_query _            _         = EmptyQuery

instance Semigroup Query where
    (<>) = modify_query

instance Monoid Query where
    mempty  = EmptyQuery
    mappend = (<>)

data Join = Join
    { join_table      :: Text
    , join_alias      :: Text
    -- , join_bindings   :: Array Text
    , join_conditions :: Condition
    }

