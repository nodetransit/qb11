{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module QueryBuilder.PostgreSql
    ( structuredQuery
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
import QueryBuilder.Internal.QueryBuilder

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

createSelect :: Query -> Text
createSelect query  = (snd . runWriter) $ do
    mapExec query [ clause_comments
                  , clause_query_type
                  , clause_columns
                  , clause_from_table
                  , clause_join
                  , clause_where_condition
                  , clause_group_by
                  , clause_having
                  , clause_order_by
                  , clause_limit
                  , clause_offset
                  ]

createUpdate :: Query -> Text
createUpdate query = (snd . runWriter) $ do
    mapExec query [ clause_comments
                  , clause_query_type
                  , clause_update_table
                  , clause_set
                  , clause_where_condition
                  , clause_returning
                  ]

createInsert :: Query -> Text
createInsert query = (snd . runWriter) $ do
    mapExec query [ clause_comments
                  , clause_query_type
                  , clause_into_table
                  , clause_insert_columns
                  , clause_insert_values
                  , clause_returning
                  ]

createDelete :: Query -> Text
createDelete query = (snd . runWriter) $ do
    mapExec query [ clause_comments
                  , clause_query_type
                  , clause_from_table
                  , clause_where_condition
                  , clause_returning
                  ]

getSelectBindings :: Query -> [Text]
getSelectBindings query = (snd . runWriter) $ do
    mapExec query [ bindings_join
                  , bindings_where_condition
                  , bindings_having
                  ]

getUpdateBindings :: Query -> [Text]
getUpdateBindings query = (snd . runWriter) $ do
    mapExec query [ bindings_set
                  , bindings_where_condition
                  ]

getInsertBindings :: Query -> [Text]
getInsertBindings query = (snd . runWriter) $ do
    mapExec query [ bindings_insert_values
                  ]

getDeleteBindings :: Query -> [Text]
getDeleteBindings query = (snd . runWriter) $ do
    mapExec query [ bindings_where_condition
                  ]

