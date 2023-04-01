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

