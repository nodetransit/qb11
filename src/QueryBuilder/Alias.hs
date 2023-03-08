module QueryBuilder.Alias
    ( Alias(..)
    ) where

import Data.Text as T
import Data.Text (Text)
import Data.String

data Alias = As Text
           | None
           deriving ( Show
                    , Eq
                    )

instance IsString (Alias) where
    fromString "" = None
    fromString s = (As . T.pack) s

