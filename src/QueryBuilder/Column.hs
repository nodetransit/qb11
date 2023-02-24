module QueryBuilder.Column
    ( Column(..)
    ) where

import Data.Text (Text)

import QueryBuilder.Alias

data Column = Column         Text
            | ColumnAlias    Text Alias
         -- | RawColumn      Text
         -- | RawColumnAlias Text Alias
            deriving ( Show
                     , Eq
                     )

