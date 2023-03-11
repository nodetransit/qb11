{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module QueryBuilder
    ( module QueryBuilder.Query
    , module QueryBuilder.Condition
    , module QueryBuilder.ToText
    , module QueryBuilder.Set
    , query
    , bindings
    ) where


import Data.Text as T hiding (map, filter, foldl)
import Data.Text (Text)
-- import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Writer

import QueryBuilder.Query
import QueryBuilder.QueryTable
import QueryBuilder.Alias
import QueryBuilder.Column
import QueryBuilder.Condition
import QueryBuilder.ToText
import QueryBuilder.Set
import QueryBuilder.QueryOrder

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

iff :: (Monoid m) => Bool -> Writer m () -> Writer m Bool
iff p f = do
    if p
      then tell $ (snd . runWriter) f
      else tell mempty
    return False

query :: Query -> Text
query EmptyQuery = ""
query query      = if | query_type query == "SELECT" -> createSelect query
                      | query_type query == "UPDATE" -> createUpdate query
                      | query_type query == "INSERT" -> createInsert query
                      | query_type query == "DELETE" -> createDelete query
                      | otherwise                    -> ""

createSelect :: Query -> Text
createSelect query  = (snd . runWriter) $ do
    tell $ query_type query
    tell $ " "
    tell $ (column_query . query_columns) query
    tell $ " FROM "
    tell $ (table_name . query_table) query
    let cond = (condition_clause . query_conditions) query
    iff (cond /= mempty) $ do
        tell $ " WHERE "
        tell $ cond
    let ordBy = (column_only . order_columns . query_orderBy) query
    iff (ordBy /= mempty) $ do
        tell $ " ORDER BY "
        tell $ column_query ordBy
        let ord = (order . query_orderBy) query
        if ord == Asc
          then tell $ " ASC"
          else tell $ " DESC"
    return True

createUpdate :: Query -> Text
createUpdate query = "u"

createInsert :: Query -> Text
createInsert query = "i"

createDelete :: Query -> Text
createDelete query = "d"


bindings :: [Text]
bindings = []

