module Fixtures.User
    ( User(..)
    , Users(..)
    ) where

import Data.Text (Text)
import Data.Text as T;
import Data.Semigroup
import Data.Time

data User = User
    { id         :: Int
    , level_id   :: Int
    , email      :: Text
    , registered :: LocalTime
    , deleted    :: Maybe LocalTime
    }

type Users = [User]


