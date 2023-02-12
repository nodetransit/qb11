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
import QueryBuilder.Order


data Column = Column         Text
            | ColumnAlias    Text Text
         -- | RawColumn      Text
         -- | RawColumnAlias Text Text
            deriving ( Show
                     , Eq
                     )

data Query = EmptyQuery
           | Select
           | Insert
           | Update
           | Delete
           | From Text
           | Table Text
           | Into Text
           | Columns [Column]
           | Where QueryCondition
           | OrderBy Order
           | GroupBy [Column]
           -- | Having QueryCondition
           | Query { query_type       :: Text
                   , query_table      :: Text
                   , query_columns    :: [Column]
                   , query_conditions :: QueryCondition
                   , query_orderBy    :: Order
                -- , query_distinct   :: [b]
                -- , query_limit      :: [n]
                -- , query_joins      :: [a]
                   , query_groupBy    :: [Column]
                -- , query_having     :: QueryCondition
               }
            deriving Show

defaultQuery = Query { query_type       = ""
                     , query_table      = ""
                     , query_columns    = []
                     , query_conditions = mempty
                     , query_orderBy    = None
                     , query_groupBy    = []
                     }

modify_query :: Query -> Query -> Query
modify_query EmptyQuery         EmptyQuery        = EmptyQuery
modify_query EmptyQuery         q                 = defaultQuery <> q
modify_query q                  EmptyQuery        = defaultQuery <> q
modify_query q@(Query {})       Select            = q { query_type = "SELECT" }
modify_query q@(Query {})       Insert            = q { query_type = "INSERT" }
modify_query q@(Query {})       Update            = q { query_type = "UPDATE" }
modify_query q@(Query {})       Delete            = q { query_type = "DELETE" }
modify_query q@(Query {})       (From t)          = q { query_table = t }
modify_query q@(Query {})       (Table t)         = q { query_table = t }
modify_query q@(Query {})       (Into t)          = q { query_table = t }
modify_query q@(Query {})       (Columns c)       = q { query_columns = c }
modify_query q@(Query {})       (Where c)         = q { query_conditions = c }
modify_query q@(Query {})       (OrderBy o)       = q { query_orderBy = o }
modify_query q@(Query {})       (GroupBy g)       = q { query_groupBy = g }
modify_query Select             q                 = defaultQuery { query_type = "SELECT" } <> q
modify_query Insert             q                 = defaultQuery { query_type = "INSERT" } <> q
modify_query Update             q                 = defaultQuery { query_type = "UPDATE" } <> q
modify_query Delete             q                 = defaultQuery { query_type = "DELETE" } <> q
modify_query (From t)           q                 = defaultQuery { query_table = t } <> q
modify_query (Table t)          q                 = defaultQuery { query_table = t } <> q
modify_query (Into t)           q                 = defaultQuery { query_table = t } <> q
modify_query (Columns c)        q                 = defaultQuery { query_columns = c } <> q
modify_query (Where c)          q                 = defaultQuery { query_conditions = c } <> q
modify_query (OrderBy o)        q                 = defaultQuery { query_orderBy = o } <> q
modify_query (GroupBy g)        q                 = defaultQuery { query_groupBy = g } <> q
modify_query qL                 qR                = coalesceQuery qL qR

coalesceQuery :: Query -> Query -> Query
coalesceQuery qL qR = Query { query_type       = queryType
                            , query_table      = queryTable
                            , query_columns    = queryColumns
                            , query_conditions = queryConditions
                            , query_orderBy    = queryOrder
                            , query_groupBy    = queryGroups
                            }
  where
    coalesce f a b = if f a /= 0 then a else b
    conditionLen = T.length . clause
    orderByLen None = 0
    orderByLen _    = 1

    queryType       = coalesce T.length       (query_type qL)       (query_type qR)
    queryTable      = coalesce T.length       (query_table qL)      (query_table qR)
    queryColumns    = coalesce Prelude.length (query_columns qL)    (query_columns qR)
    queryConditions = coalesce conditionLen   (query_conditions qL) (query_conditions qR)
    queryOrder      = coalesce orderByLen     (query_orderBy qL)    (query_orderBy qR)
    queryGroups     = coalesce Prelude.length (query_groupBy qL)    (query_groupBy qR)

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

