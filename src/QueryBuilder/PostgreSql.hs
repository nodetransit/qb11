{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module QueryBuilder.PostgreSql
    ( query
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
import QueryBuilder.Internal.QueryBuilder


query :: Query -> Text
query EmptyQuery = ""
query query      = if | query_type query == "SELECT" -> createSelect query
                      | query_type query == "UPDATE" -> createUpdate query
                      | query_type query == "INSERT" -> createInsert query
                      | query_type query == "DELETE" -> createDelete query
                      | otherwise                    -> ""

bindings :: Query -> [Text]
bindings EmptyQuery = []
bindings query      = if | query_type query == "SELECT" -> getBindings query
                         | query_type query == "UPDATE" -> getBindings query
                         | query_type query == "INSERT" -> getBindings query
                         | query_type query == "DELETE" -> getBindings query
                         | otherwise                    -> []

createSelect :: Query -> Text
createSelect query  = (snd . runWriter) $ do
    mapExec query [ clause_comments
                  , clause_query_type
                  , clause_columns
                  , clause_from_table
                  , clause_where_condition
                  , clause_order_by
                  , clause_group_by
                  , clause_having
                  , clause_limit
                  ]

createUpdate :: Query -> Text
createUpdate query = "u"

createInsert :: Query -> Text
createInsert query = "i"

createDelete :: Query -> Text
createDelete query = "d"




getBindings _ = []
