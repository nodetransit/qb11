{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS -Wall
            -Wno-missing-fields
            -Wno-unused-imports
            -Wno-unused-top-binds
            #-}

module QueryBuilder
    ( Query (..)
    , Join (..)
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

