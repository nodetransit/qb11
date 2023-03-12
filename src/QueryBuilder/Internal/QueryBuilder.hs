{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}

module QueryBuilder.Internal.QueryBuilder
    ( column_only
    , column_query
    , iff
    , mapExec
    , clause_query_type
    , clause_columns
    , clause_from_table
    , clause_where_condition
    , clause_order_by
    ) where


import Data.Text as T hiding (map, filter, foldl)
import Data.Text (Text)
import Control.Monad.Writer

import QueryBuilder.Query
import QueryBuilder.QueryTable
import QueryBuilder.Alias
import QueryBuilder.Column
import QueryBuilder.Condition
import QueryBuilder.ToText
import QueryBuilder.Set
import QueryBuilder.QueryOrder

type Clause = Query -> Writer Text ()

column_only :: [Column] -> [Column]
column_only = filter f
  where
    f c@(Column _) = True
    f _            = False

column_query :: [Column] -> Text
column_query = join . toColumns
  where
    join :: [Text] -> Text
    join = T.intercalate ", "

    toColumns :: [Column] -> [Text]
    toColumns = map toColumn

    toColumn :: Column -> Text
    toColumn (Column c)             = c
    toColumn (ColumnAlias c (As a)) = c <> " AS " <> a

iff :: (Monoid m) => Bool -> Writer m () -> Writer m ()
iff p f = do
    if p
      then tell $ (snd . runWriter) f
      else tell mempty
    return ()

-- | map the query to a list of functions
mapExec :: (Traversable t, Monad m) => Query -> t (Query -> m w) -> m (t w)
mapExec query = mapM (\f -> f query)

clause_query_type :: Clause
clause_query_type = tell . query_type

clause_columns :: Clause
clause_columns query = do
    tell $ " "
    tell $ (column_query . query_columns) query

clause_from_table :: Clause
clause_from_table query = do
    tell $ " FROM "
    tell $ (table_name . query_table) query

clause_into_table :: Clause
clause_into_table query = do
    tell $ " INTO "
    tell $ (table_name . query_table) query

clause_where_condition :: Clause
clause_where_condition query = do
    let cond = (condition_clause . query_conditions) query
    iff (cond /= mempty) $ do
        tell $ " WHERE "
        tell $ cond

clause_order_by :: Clause
clause_order_by query = do
    let ordBy = (column_only . order_columns . query_orderBy) query
    iff (ordBy /= mempty) $ do
        tell $ " ORDER BY "
        tell $ column_query ordBy
        let ord = (order . query_orderBy) query
        if ord == Asc
          then tell $ " ASC"
          else tell $ " DESC"

