{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module QueryBuilder.Query
    ( Query(..)
    , Column(..)
    , defaultQuery
    ) where

import Data.Text as T
import Data.Text (Text)
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
           | TableAlias Text Text
           | Into Text
           | Distinct
           | Columns [Column]
        -- | Values [Column]
           | GroupBy [Column]
           | Having QueryCondition
        -- | Join Text QueryCondition
        -- | Join Alias Text Text QueryCondition
           | Where QueryCondition
           | OrderBy [Column] Order
           | Limit Int
           | Query { query_type       :: Text
                   , query_table      :: Text
                   , query_distinct   :: Bool
                   , query_columns    :: [Column]
                -- , query_values     :: [Column]
                   , query_groupBy    :: [Column]
                   , query_having     :: QueryCondition
                -- , query_joins      :: [a]
                   , query_conditions :: QueryCondition
                   , query_orderBy    :: ([Column], Order)
                   , query_limit      :: Maybe Int
               }
            deriving Show

-- | Default Query with empty values
defaultQuery = Query { query_type       = ""
                     , query_table      = ""
                     , query_distinct   = False
                     , query_columns    = []
                  -- , query_values     = []
                     , query_groupBy    = []
                     , query_having     = mempty
                  -- , query_joins      = []
                     , query_conditions = mempty
                     , query_orderBy    = ([], None)
                     , query_limit      = Nothing
                     }

-- | Concatenate Queries
--
--   modify existing query if the property is empty
--   if the property is not empty, the current value
--   is not modified
modify_query :: Query -> Query -> Query
modify_query = mq
  where
    mq EmptyQuery         EmptyQuery        = EmptyQuery
    mq EmptyQuery         q                 = defaultQuery <> q
    mq q                  EmptyQuery        = defaultQuery <> q
    mq q@(Query {})       Select            = q { query_type = "SELECT" }
    mq q@(Query {})       Insert            = q { query_type = "INSERT" }
    mq q@(Query {})       Update            = q { query_type = "UPDATE" }
    mq q@(Query {})       Delete            = q { query_type = "DELETE" }
    mq q@(Query {})       (From t)          = q { query_table = t }
    mq q@(Query {})       (Table t)         = q { query_table = t }
    mq q@(Query {})       (Into t)          = q { query_table = t }
    mq q@(Query {})       (Columns c)       = q { query_columns = c }
    mq q@(Query {})       (Where c)         = q { query_conditions = c }
    mq q@(Query {})       (OrderBy c o)     = q { query_orderBy = (c, o) }
    mq q@(Query {})       (GroupBy g)       = q { query_groupBy = g }
    mq q@(Query {})       (Having c)        = q { query_having = c }
    mq q@(Query {})       Distinct          = q { query_distinct = True }
    mq q@(Query {})       (Limit n)         = q { query_limit = Just n }
    mq Select             q                 = defaultQuery { query_type = "SELECT" } <> q
    mq Insert             q                 = defaultQuery { query_type = "INSERT" } <> q
    mq Update             q                 = defaultQuery { query_type = "UPDATE" } <> q
    mq Delete             q                 = defaultQuery { query_type = "DELETE" } <> q
    mq (From t)           q                 = defaultQuery { query_table = t } <> q
    mq (Table t)          q                 = defaultQuery { query_table = t } <> q
    mq (Into t)           q                 = defaultQuery { query_table = t } <> q
    mq (Columns c)        q                 = defaultQuery { query_columns = c } <> q
    mq (Where c)          q                 = defaultQuery { query_conditions = c } <> q
    mq (OrderBy c o)      q                 = defaultQuery { query_orderBy = (c, o) } <> q
    mq (GroupBy g)        q                 = defaultQuery { query_groupBy = g } <> q
    mq (Having c)         q                 = defaultQuery { query_having = c } <> q
    mq Distinct           q                 = defaultQuery { query_distinct = True } <> q
    mq (Limit n)          q                 = defaultQuery { query_limit = Just n } <> q
    mq qL                 qR                = coalesceQuery qL qR

-- | Merge Queries
coalesceQuery :: Query -> Query -> Query
coalesceQuery qL qR = Query { query_type       = queryType
                            , query_table      = queryTable
                            , query_distinct   = queryDistinct
                            , query_columns    = queryColumns
                         -- , query_values     = queryValues
                            , query_groupBy    = queryGroups
                            , query_having     = queryHaving
                         -- , query_joins      = queryJoins
                            , query_conditions = queryConditions
                            , query_orderBy    = queryOrder
                            , query_limit      = queryLimit
                            }
  where
    coalesce f g = if (f . g) qL /= 0 then g qL else g qR

    conditionLen = T.length . clause
    orderByLen (cs, _) = Prelude.length cs
    distinctLen b = if b == True then 1 else 0
    limitLen Nothing = 0
    limitLen _       = 1

    queryType       = coalesce T.length       query_type
    queryTable      = coalesce T.length       query_table
    queryColumns    = coalesce Prelude.length query_columns
    queryConditions = coalesce conditionLen   query_conditions
    queryOrder      = coalesce orderByLen     query_orderBy
    queryGroups     = coalesce Prelude.length query_groupBy
    queryHaving     = coalesce conditionLen   query_having
    queryDistinct   = coalesce distinctLen    query_distinct
    queryLimit      = coalesce limitLen       query_limit

instance Semigroup Query where
    (<>) :: Query -> Query -> Query
    (<>) = modify_query

instance Monoid Query where
    mempty :: Query
    mempty = EmptyQuery

    mappend :: Query -> Query -> Query
    mappend = (<>)

