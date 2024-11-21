module QueryBuilder.Using
    ( Using(..)
    ) where

import Data.Text as T
import Data.Text (Text)
import Data.String
import Data.Semigroup

import QueryBuilder.Column

data Using = Using [Text]
           deriving ( Show
                    , Eq
                    )

