module QueryBuilder.Tristate
    ( Tristate(..)
    ) where

data Tristate = Undefined
              | True
              | False
              deriving ( Show
                       , Eq
                       )


