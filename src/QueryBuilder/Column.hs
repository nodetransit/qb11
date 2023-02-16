module QueryBuilder.Column
    ( Column(..)
    , Alias
    ) where

import Data.Text (Text)

import QueryBuilder

data Column = Column         Text
            | ColumnAlias    Text Alias
         -- | RawColumn      Text
         -- | RawColumnAlias Text Alias
            deriving ( Show
                     , Eq
                     )

