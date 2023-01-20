{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module QueryBuilder
    ( Query (..)
    , Join (..)
    , Column (..)
    , createQuery
    ) where

import Data.Text as T
import Data.Text (Text)
import Data.Array as Array
import QueryBuilder.Types


data QueryBuilderT q m b = QueryBuilderT { runQueryBuilderT :: m (q, b) }

createQuery :: Text -> Query
createQuery tableName = Query { query_table      = tableName
                              -- , query_conditions = []
                              }

