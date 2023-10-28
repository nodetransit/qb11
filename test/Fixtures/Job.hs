module Fixtures.Job
    ( Job(..)
    ) where

import Data.Text (Text)
import Data.Text as T;
import Data.Semigroup
import Data.Time

data Job = Job
    { id            :: Int
    , job_type_id   :: Int
    , user_id       :: Int
    , date          :: LocalTime
    , successfull   :: Maybe Bool
    , retries       :: Maybe Int
    , cancelled     :: Maybe LocalTime
    , cancel_reason :: Maybe Text
    , failed        :: Maybe LocalTime
    , fail_reason   :: Maybe Text
    }

