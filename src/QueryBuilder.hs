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


import Data.Text as T
import Data.Text (Text)
-- import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Writer

import QueryBuilder.Query
import QueryBuilder.Condition
import QueryBuilder.ToText
import QueryBuilder.Set

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
    return True

createUpdate :: Query -> Text
createUpdate query = "u"

createInsert :: Query -> Text
createInsert query = "i"

createDelete :: Query -> Text
createDelete query = "d"


bindings :: [Text]
bindings = []

