{-# LANGUAGE OverloadedStrings #-}

module QueryBuilder.Raw
    ( Raw(..)
    , Value(..)
    ) where

import Data.Text as T
import Data.Text (Text)
import Data.String

data Raw = Raw Text
         deriving Show

data Value = Value
     { text :: Text
     , bind :: Text
     }
     deriving Show

instance IsString (Raw) where
    fromString = Raw . T.pack

instance IsString (Value) where
    fromString a = Value { text = "?", bind = T.pack a }

