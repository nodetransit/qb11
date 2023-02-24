module QueryBuilder.Alias
    ( Alias(..)
    ) where

import Data.Text (Text)

data Alias = As Text
           | None
           deriving ( Show
                    , Eq
                    )

