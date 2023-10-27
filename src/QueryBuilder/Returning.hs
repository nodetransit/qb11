module QueryBuilder.Returning
    ( Returning(..)
    ) where

import Data.Text as T
import Data.Text (Text)
import Data.String
import Data.Semigroup

import QueryBuilder.Column

data Returning = Into [Column]
               | Nothing
               deriving ( Show
                        , Eq
                        )
