{-# LANGUAGE OverloadedStrings #-}

module QueryBuilder.Set
    ( X(..)
    , clause
    , bindings
    ) where

import Data.Text as T hiding (map, filter)

data X = X Text Text
       | XRaw Text Text

clause :: [X] -> Text
clause a = T.intercalate "," $ map g a
  where
    g (X c _) = c <> " = ?"
    g (XRaw c v) = c <> " = " <> v

bindings :: [X] -> [Text]
bindings a = map f $ filter g a
  where
    g (X _ _) = True
    g (XRaw _ _) = False

    f (X _ v) = v
