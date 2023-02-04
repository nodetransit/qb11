module QueryBuilder.Operator
    ( Operator(..)
    ) where

data Operator = Equals
               | NotEquals
               | Is
               | IsNot
               | Not
            -- | Null
            -- | NotNull
               | Like
               | NotLike

instance Show Operator where
    show Equals    = "="
    show NotEquals = "<>"
    show Is        = "IS"
    show IsNot     = "IS NOT"
    show Not       = "NOT"
    -- show Null      = "IS NULL"
    -- show NotNull   = "IS NOT NULL"
    show Like      = "LIKE"
    show NotLike   = "NOT LIKE"

