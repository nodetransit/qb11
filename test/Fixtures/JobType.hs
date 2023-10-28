module Fixtures.JobType
    ( JobType(..)
    ) where

import Data.Text (Text)
import Data.Text as T;
import Data.Semigroup
import Data.Time

data JobType = JobType
    { id          :: Int
    , name        :: Maybe Text
    , description :: Maybe Text
    }

