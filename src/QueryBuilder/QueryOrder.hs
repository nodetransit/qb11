module QueryBuilder.QueryOrder
    ( QueryOrder(..)
    , Order(..)
    ) where

import QueryBuilder.Column

data QueryOrder = QueryOrder
    { columns :: [Column]
    , order   :: Order
    }
    deriving ( Show
             , Eq
             )

data Order= Asc
          | Desc
          | None
          deriving ( Show
                   , Eq
                   )

