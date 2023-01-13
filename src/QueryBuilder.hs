{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module QueryBuilder
    ( Query (..)
    , createQuery
    ) where

import Data.Text as T
import Data.Text (Text)


newtype Query = Query
    { table      :: Text
    , conditions :: Condition
    , bindings   :: [a]
    , orderBy    :: [a]
    , distinct   :: [b]
    , limit      :: [n]
    , joins      :: [a]
    }

newtype Join = Join
    { table      :: Text
    , alias      :: Text
    , bindings   :: [Text]
    , conditions :: Condition
    }

data Condition = Condition | [Condition] | and | or

createQuery :: Text -> Query
createQuery table = Query { table      = table
                          , conditions = []
                          }

