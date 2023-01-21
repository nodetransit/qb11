{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module QueryBuilder.Types
    ( Query(..)
    , Column(..)
    , select
    , insert
    , update
    , delete
    , from
    , into
    , table
    , columns
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
            deriving Show

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
           | Table Text
           | Columns [Column]
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
            deriving Show

defaultQuery = Query { query_type    = ""
                     , query_table   = ""
                     , query_columns = []
                     }

select = defaultQuery { query_type = "SELECT" }
insert = defaultQuery { query_type = "INSERT" }
update = defaultQuery { query_type = "UPDATE" }
delete = defaultQuery { query_type = "DELETE" }

from = Table
into = Table
table = Table

columns = Columns

modify_query :: Query -> Query -> Query
modify_query EmptyQuery     q              = q
modify_query q              EmptyQuery     = q
modify_query q@(Query {})   (Table t)      = q { query_table = t }
modify_query q@(Query {})   (Columns c)    = q { query_columns = c }
modify_query (Columns c)    q              = defaultQuery { query_columns = c } <> q
modify_query (Table t)      q              = defaultQuery { query_table = t } <> q
modify_query qL             qR             = coalesceQuery qL qR

coalesceQuery :: Query -> Query -> Query
coalesceQuery qL qR = Query { query_type    = queryType
                            , query_table   = queryTable
                            , query_columns = queryColumns
                            }
  where
    coalesce f a b = if f a /= 0 then a else b

    queryType    = coalesce T.length (query_type qL) (query_type qR)
    queryTable   = coalesce T.length (query_table qL) (query_table qR)
    queryColumns = coalesce Prelude.length (query_columns qL) (query_columns qR)

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

