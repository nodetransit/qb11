{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module QueryBuilder.Query
    ( 
    ) where

import Data.Text as T
import Data.Text (Text)
import Control.Monad
import Control.Applicative

import QueryBuilder.Internal.Query

