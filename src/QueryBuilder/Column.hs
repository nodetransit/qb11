module QueryBuilder.Column
    ( Column(..)
    ) where

import Data.Text as T
import Data.Text (Text)
import Data.String

import QueryBuilder.Alias

data Column = Column         Text
            | ColumnAlias    Text Alias
         -- | RawColumn      Text
         -- | RawColumnAlias Text Alias
            deriving ( Show
                     , Eq
                     )

instance IsString (Column) where
    fromString = Column . T.pack

