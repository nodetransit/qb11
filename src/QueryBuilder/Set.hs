{-# LANGUAGE OverloadedStrings #-}

module QueryBuilder.Set
    ( SetValue(..)
    , set_clause
    , set_bindings
    , (.=)
    ) where

import Data.Text as T hiding (length, map, filter, foldl)
import Data.Semigroup

import QueryBuilder.ToText

data SetValue = SetValue Text Text Text
              deriving Show

set_clause :: [SetValue] -> Text
set_clause a = T.intercalate ", " $ map g a
  where
    g (SetValue c v b) = c <> " = " <> (if b == mempty then v else "?")

set_bindings :: [SetValue] -> [Text]
set_bindings a = foldl f [] $ filter g a
  where
    g (SetValue _ _ v) = v /= mempty

    f ax (SetValue _ _ v) = ax <> [v]

infixl 8 .=
(.=) :: (ToText t) => Text -> t -> SetValue
(.=) col v = SetValue col val bind
  where
    val  = toText v
    bind = toBind v

