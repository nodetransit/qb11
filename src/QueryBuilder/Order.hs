module QueryBuilder.Order
    ( Order(..)
    ) where

data Order = Asc
           | Desc
           | None
           deriving ( Show
                    , Eq
                    )

