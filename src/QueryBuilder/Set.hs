{-# LANGUAGE OverloadedStrings #-}

module QueryBuilder.Set
    ( SetValue(..)
    , clause
    , bindings
    ) where

import Data.Text as T hiding (map, filter)
import Data.Semigroup

data SetValue = SetValue Text Text
              | SetValueRaw Text Text

clause :: [SetValue] -> Text
clause a = T.intercalate "," $ map g a
  where
    g (SetValue c _) = c <> " = ?"
    g (SetValueRaw c v) = c <> " = " <> v

bindings :: [SetValue] -> [Text]
bindings a = map f $ filter g a
  where
    g (SetValue _ _) = True
    g (SetValueRaw _ _) = False

    f (SetValue _ v) = v

-- (:=) :: Text -> Text -> SetValue
-- (:=) col val = SetValue col val

-- (!=) :: Text -> Text -> SetValue
-- (!=) col val = SetValueRaw col val

