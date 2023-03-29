{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}

module QueryBuilder.Internal.QueryBuilder
    ( column_only
    , column_query
    , iff
    , mapExec
    , clause_comments
    , clause_query_type
    , clause_columns
    , clause_from_table
    , clause_into_table
    , clause_update_table
    , clause_insert_columns
    , clause_insert_values
    , clause_set
    , clause_join
    , clause_where_condition
    , clause_group_by
    , clause_having
    , clause_order_by
    , clause_limit
    , clause_offset
    , clause_returning

    , bindings_join
    , bindings_insert_values
    , bindings_set
    , bindings_where_condition
    , bindings_having
    ) where


import Data.Text as T hiding (map, filter, foldl)
import Data.Text (Text)
import Control.Monad.Writer

import QueryBuilder.Query
import QueryBuilder.QueryTable
import QueryBuilder.Alias as Alias
import QueryBuilder.Column
import QueryBuilder.Condition
import QueryBuilder.ToText
import QueryBuilder.Set
import QueryBuilder.QueryOrder
import QueryBuilder.JoinTable

type Clause = Query -> Writer Text ()
type Bindings = Query -> Writer [Text] ()

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

clause_comments :: Clause
clause_comments query = do
    let comments = query_comments query
    iff (comments /= mempty) $ do
        mapM p comments
        tell mempty
  where
    p c = do
        tell $ "-- "
        tell $ c
        tell $ "\n"

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

clause_update_table :: Clause
clause_update_table query = do
    tell $ " "
    tell $ (table_name . query_table) query

clause_insert_columns :: Clause
clause_insert_columns query = do
    tell $ " ("
    tell $ (column_query . column_only . query_columns) query
    tell $ ")"

clause_insert_values :: Clause
clause_insert_values query = do
    tell $ " VALUES "
    tell $ (condition_clause . query_values) query

clause_set :: Clause
clause_set query = do
    tell $ " SET "
    tell $ (set_clause . query_set) query

clause_where_condition :: Clause
clause_where_condition query = do
    let cond = (condition_clause . query_conditions) query
    iff (cond /= mempty) $ do
        tell $ " WHERE "
        tell $ cond

clause_join :: Clause
clause_join query = do
    let joins = query_joins query
    iff (joins /= mempty) $ do
        mapM mkJoin joins
        tell mempty
  where
    mkJoin q = do
        tell $ " "
        tell $ (T.pack . show . join_type) q
        tell $ " JOIN "
        tell $ join_table q
        let alias = join_alias q
        case alias of
            Alias.None -> tell mempty
            As a       -> do
                tell $ " AS "
                tell $ a
        tell $ " ON "
        tell $ (condition_clause . join_conditions) q

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

clause_group_by :: Clause
clause_group_by query = do
    let grpBy = (column_only . query_groupBy) query
    iff (grpBy /= mempty) $ do
        tell $ " GROUP BY "
        tell $ column_query grpBy

clause_having :: Clause
clause_having query = do
    let cond = (condition_clause . query_having) query
    iff (cond /= mempty) $ do
        tell $ " HAVING ( "
        tell $ cond
        tell $ " )"

clause_limit :: Clause
clause_limit query = do
    let limit = query_limit query
    tell mempty
    case limit of
        Nothing -> tell mempty
        Just n  -> do
            tell $ " LIMIT "
            tell $ (T.pack . show) n

clause_offset :: Clause
clause_offset query = do
    let offset = query_offset query
    tell mempty
    case offset of
        Nothing -> tell mempty
        Just n  -> do
            tell $ " OFFSET "
            tell $ (T.pack . show) n

clause_returning :: Clause
clause_returning query = do
    let ret = query_returning query
    iff (ret /= mempty) $ do
        tell " RETURNING "
        tell ret

bindings_insert_values :: Bindings
bindings_insert_values query = do
    tell $ (condition_bindings . query_values) query

bindings_set :: Bindings
bindings_set query = do
    tell $ (set_bindings . query_set) query

bindings_join :: Bindings
bindings_join query = do
    let joins = query_joins query
    iff (joins /= mempty) $ do
        mapM mkJoinBindings joins
        tell mempty
  where
    mkJoinBindings q = tell $ (condition_bindings . join_conditions) q

bindings_where_condition :: Bindings
bindings_where_condition query = do
    let bindings = (condition_bindings . query_conditions) query
    iff (bindings /= mempty) $ tell bindings

bindings_having :: Bindings
bindings_having query = do
    let bindings = (condition_bindings . query_having) query
    iff (bindings /= mempty) $ tell bindings

