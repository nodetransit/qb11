{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module QueryBuilder.Condition
    ( Condition(..)
    , Operation(..)
    ) where

import Data.Text as T
import Data.Text (Text)
import Data.Array as Array
import Control.Monad
import Control.Applicative

data Condition = Condition String Operation String
               | Group [Condition]
             deriving Show
-- data Condition = Condition Text | Array Condition

data Operation = Equals
               | NotEquals
               | Null
               | NotNull
               | Like
               | NotLike
             deriving Show

and :: Bool
and = True

or :: Bool
or = False
