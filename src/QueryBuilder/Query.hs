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
        -- | Distinct
           | Columns [Column]
        -- | Values [Column]
           | GroupBy [Column]
           | Having QueryCondition
        -- | Join Text QueryCondition
        -- | Join Alias Text Text QueryCondition
           | Where QueryCondition
           | OrderBy [Column] Order
        -- | Limit Int
           | Query { query_type       :: Text
                   , query_table      :: Text
                -- , query_distinct   :: Bool
                   , query_columns    :: [Column]
                -- , query_values     :: [Column]
                   , query_groupBy    :: [Column]
                   , query_having     :: QueryCondition
                -- , query_joins      :: [a]
                   , query_conditions :: QueryCondition
                   , query_orderBy    :: ([Column], Order)
                -- , query_limit      :: [n]
               }
            deriving Show

-- | Default Query with empty values
defaultQuery = Query { query_type       = ""
                     , query_table      = ""
                     , query_columns    = []
                     , query_conditions = mempty
                     , query_orderBy    = ([], None)
                     , query_groupBy    = []
                     , query_having     = mempty
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
    mq qL                 qR                = coalesceQuery qL qR

-- | Merge Queries
coalesceQuery :: Query -> Query -> Query
coalesceQuery qL qR = Query { query_type       = queryType
                            , query_table      = queryTable
                            , query_columns    = queryColumns
                            , query_conditions = queryConditions
                            , query_orderBy    = queryOrder
                            , query_groupBy    = queryGroups
                            , query_having     = queryHaving
                            }
  where
    coalesce f a b = if f a /= 0 then a else b
    conditionLen = T.length . clause
    orderByLen (cs, _) = Prelude.length cs

    queryType       = coalesce T.length       (query_type qL)       (query_type qR)
    queryTable      = coalesce T.length       (query_table qL)      (query_table qR)
    queryColumns    = coalesce Prelude.length (query_columns qL)    (query_columns qR)
    queryConditions = coalesce conditionLen   (query_conditions qL) (query_conditions qR)
    queryOrder      = coalesce orderByLen     (query_orderBy qL)    (query_orderBy qR)
    queryGroups     = coalesce Prelude.length (query_groupBy qL)    (query_groupBy qR)
    queryHaving     = coalesce conditionLen   (query_having qL)     (query_having qR)

instance Semigroup Query where
    (<>) :: Query -> Query -> Query
    (<>) = modify_query

instance Monoid Query where
    mempty :: Query
    mempty = EmptyQuery

    mappend :: Query -> Query -> Query
    mappend = (<>)

