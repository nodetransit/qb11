module QueryBuilder.Raw
    ( Raw(..)
    ) where

import Data.Text as T
import Data.Text (Text)
import Data.String

data Raw = Raw Text
         deriving Show

instance IsString (Raw) where
    fromString = Raw . T.pack

