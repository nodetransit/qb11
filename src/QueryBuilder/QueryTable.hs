{-# LANGUAGE OverloadedStrings #-}

module QueryBuilder.QueryTable
    ( QueryTable(..)
    ) where

import Data.Text (Text)

import QueryBuilder.Alias

data QueryTable = QueryTable
    { table_name  :: Text
    , table_alias :: Alias
    }
    deriving ( Show
             , Eq
             )

