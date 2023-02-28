{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE IncoherentInstances #-}

module QueryBuilder.ToString
    ( ToString(..)
    ) where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as BL
import qualified Data.ByteString.Short as BS

import qualified Data.ByteString.Char8      as B.Char8
import qualified Data.ByteString.Lazy.Char8 as BL.Char8
import qualified Data.ByteString.Short      as BS.Short

import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Encoding      as TE
import qualified Data.Text.Lazy.Encoding as TLE

import QueryBuilder.ToText

data TrueType

data FalseType

type family TypeEqF a b where
    TypeEqF a a = TrueType
    TypeEqF a b = FalseType

type TypeNeq a b = TypeEqF a b ~ FalseType

class ToString a where
    toString :: a -> String

instance ToString String where
    toString = id

instance ToString B.Char8.ByteString where
    toString = B.Char8.unpack

instance ToString BL.Char8.ByteString where
    toString = BL.Char8.unpack

instance ToString BS.Short.ShortByteString where
    toString = toString . BS.Short.fromShort

instance ToString T.Text where
    toString = T.unpack

instance ToString TL.Text where
    toString = TL.unpack

-- (All Show instances can be ToString)
instance ( Show a
         , TypeNeq a String
         , TypeNeq a B.Char8.ByteString
         , TypeNeq a BL.Char8.ByteString
         , TypeNeq a BS.Short.ShortByteString
         , TypeNeq a T.Text
         , TypeNeq a TL.Text
         ) => ToString a
      where
        toString = show 


