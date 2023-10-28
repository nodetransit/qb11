module Fixtures.UserInfo
    ( UserInfo(..)
    ) where

import Data.Text (Text)
import Data.Text as T;
import Data.Semigroup
import Data.Time

data UserInfo = UserInfo
    { id        :: Int
    , user_id   :: Int
    , name      :: Text
    , country   :: Maybe Text
    , address   :: Maybe Text
    , telephone :: Maybe Text
    }

