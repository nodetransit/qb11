{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module QueryBuilder.Query
    ( Query(..)
    , Column(..)
    , Join(..)
    , defaultQuery
    ) where

import Data.Text as T
import Data.Text (Text)
import Data.Array as Array
import Control.Monad
import Control.Applicative

import QueryBuilder.Condition


data Column = Column         Text
            | ColumnAlias    Text Text
         -- | RawColumn      Text
         -- | RawColumnAlias Text Text
            deriving Show

data Query = EmptyQuery
           | Select
           | Insert
           | Update
           | Delete
           | From Text
           | Table Text
           | Into Text
           | Columns [Column]
           | Query { query_type       :: Text
                   , query_table      :: Text
                   , query_columns    :: [Column]
                   , query_conditions :: (Text, [Text])
                -- , query_orderBy    :: [a]
                -- , query_distinct   :: [b]
                -- , query_limit      :: [n]
                -- , query_joins      :: [a]
                -- , query_having     :: (Text, [Text])
                -- , query_group      :: [Text]
               }
            deriving Show

defaultQuery = Query { query_type       = ""
                     , query_table      = ""
                     , query_columns    = []
                     , query_conditions = ("", [])
                     }

modify_query :: Query -> Query -> Query
modify_query EmptyQuery     q              = defaultQuery <> q
modify_query q              EmptyQuery     = defaultQuery <> q
modify_query q@(Query {})   Select         = q { query_type = "SELECT" }
modify_query q@(Query {})   Insert         = q { query_type = "INSERT" }
modify_query q@(Query {})   Update         = q { query_type = "UPDATE" }
modify_query q@(Query {})   Delete         = q { query_type = "DELETE" }
modify_query q@(Query {})   (From t)       = q { query_table = t }
modify_query q@(Query {})   (Table t)      = q { query_table = t }
modify_query q@(Query {})   (Into t)       = q { query_table = t }
modify_query q@(Query {})   (Columns c)    = q { query_columns = c }
modify_query Select         q              = defaultQuery { query_type = "SELECT" } <> q
modify_query Insert         q              = defaultQuery { query_type = "INSERT" } <> q
modify_query Update         q              = defaultQuery { query_type = "UPDATE" } <> q
modify_query Delete         q              = defaultQuery { query_type = "DELETE" } <> q
modify_query (Columns c)    q              = defaultQuery { query_columns = c } <> q
modify_query (From t)       q              = defaultQuery { query_table = t } <> q
modify_query (Table t)      q              = defaultQuery { query_table = t } <> q
modify_query (Into t)       q              = defaultQuery { query_table = t } <> q
modify_query qL             qR             = coalesceQuery qL qR

coalesceQuery :: Query -> Query -> Query
coalesceQuery qL qR = Query { query_type       = queryType
                            , query_table      = queryTable
                            , query_columns    = queryColumns
                            , query_conditions = queryConditions
                            }
  where
    coalesce f a b = if f a /= 0 then a else b
    conditionLen (p, _) = T.length p

    queryType       = coalesce T.length       (query_type qL)       (query_type qR)
    queryTable      = coalesce T.length       (query_table qL)      (query_table qR)
    queryColumns    = coalesce Prelude.length (query_columns qL)    (query_columns qR)
    queryConditions = coalesce conditionLen   (query_conditions qL) (query_conditions qR)

instance Semigroup Query where
    (<>) :: Query -> Query -> Query
    (<>) = modify_query

instance Monoid Query where
    mempty :: Query
    mempty = EmptyQuery

    mappend :: Query -> Query -> Query
    mappend = (<>)

data Join = Join
    { join_table      :: Text
    , join_alias      :: Text
    , join_conditions :: (Text, [Text])
    }

