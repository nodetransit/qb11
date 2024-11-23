{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module QueryBuilder.SqlServer
    ( structuredQuery
    , query
    , bindings
    ) where


import Data.Text as T hiding (map, filter, foldl)
import Data.Text.Encoding as T
import Data.Text (Text)
import Data.ByteString as B
import Data.ByteString (ByteString)
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
import qualified QueryBuilder.Returning as Returning
import QueryBuilder.Internal.QueryBuilder hiding
    ( createUpdate
    , createSelect
    , createInsert
    , createDelete
    , clause_limit
    , clause_returning
    )

structuredQuery :: Query -> ByteString
structuredQuery = T.encodeUtf8 . query

query :: Query -> Text
query EmptyQuery = ""
query query      = if | query_type query == "SELECT" -> createSelect query
                      | query_type query == "UPDATE" -> createUpdate query
                      | query_type query == "INSERT" -> createInsert query
                      | query_type query == "DELETE" -> createDelete query
                      | otherwise                    -> ""

bindings :: Query -> [Text]
bindings EmptyQuery = []
bindings query      = if | query_type query == "SELECT" -> getSelectBindings query
                         | query_type query == "UPDATE" -> getUpdateBindings query
                         | query_type query == "INSERT" -> getInsertBindings query
                         | query_type query == "DELETE" -> getDeleteBindings query
                         | otherwise                    -> []

clause_limit :: Clause
clause_limit query = do
    let limit = query_limit query
    tell mempty
    case limit of
        Nothing -> tell mempty
        Just n  -> do
            tell $ " TOP ("
            tell $ (T.pack . show) n
            tell $ ")"

returning_query :: [Column] -> Text
returning_query = join . getCols
  where
    getCols :: [Column] -> [Text]
    getCols = Prelude.map splitCols

    join :: [Text] -> Text
    join = T.intercalate ", "

    splitCols c = case c of
        Column t             -> t
        ColumnAlias t (As a) -> t <> " AS " <> a

clause_returning_inserted :: Clause
clause_returning_inserted query = do
    let ret = query_returning query
    case ret of
        Returning.Into c -> do
            tell $ " OUTPUT "
            tell $ returning_query $ Prelude.map appendInsert c
        _                -> tell mempty
  where
    appendInsert c = case c of
        Column t        -> Column ("Inserted." <> t)
        ColumnAlias t a -> ColumnAlias ("Inserted." <> t) a

clause_returning_deleted :: Clause
clause_returning_deleted query = do
    let ret = query_returning query
    case ret of
        Returning.Into c -> do
            tell $ " OUTPUT "
            tell $ returning_query $ Prelude.map appendDelete c
        _                -> tell mempty
  where
    appendDelete c = case c of
        Column t        -> Column ("Deleted." <> t)
        ColumnAlias t a -> ColumnAlias ("Deleted." <> t) a

createSelect :: Query -> Text
createSelect query  = (snd . runWriter) $ do
    mapExec query [ clause_comments
                  , clause_query_type
                  , clause_distinct
                  , clause_limit
                  , clause_columns
                  , clause_from_table
                  , clause_join
                  , clause_where_condition
                  , clause_group_by
                  , clause_having
                  , clause_order_by
                  ]

createInsert :: Query -> Text
createInsert query = (snd . runWriter) $ do
    mapExec query [ clause_comments
                  , clause_query_type
                  , clause_into_table
                  , clause_insert_columns
                  , clause_returning_inserted
                  , clause_insert_values
                  ]

createUpdate :: Query -> Text
createUpdate query = (snd . runWriter) $ do
    mapExec query [ clause_comments
                  , clause_query_type
                  , clause_limit
                  , clause_update_table
                  , clause_set
                  , clause_returning_inserted
                  , clause_where_condition
                  ]

createDelete :: Query -> Text
createDelete query = (snd . runWriter) $ do
    mapExec query [ clause_comments
                  , clause_query_type
                  , clause_from_table
                  , clause_returning_deleted
                  , clause_where_condition
                  ]

