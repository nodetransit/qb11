{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE IncoherentInstances #-}

module QueryBuilder.ToText
    ( ToText(..)
    , module QueryBuilder.Raw
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

import QueryBuilder.Raw

class (Show a) => ToText a where
    toText :: a -> T.Text
    toBind :: a -> T.Text

instance ToText Char where
    toText _ = T.pack "?"
    toBind = T.pack . show

instance ToText String where
    toText _ = T.pack "?"
    toBind = T.pack

instance ToText B.Char8.ByteString where
    toText _ = T.pack "?"
    toBind = TE.decodeUtf8

instance ToText BL.Char8.ByteString where
    toText _ = T.pack "?"
    toBind = TE.decodeUtf8 . B.concat . BL.toChunks

instance ToText BS.Short.ShortByteString where
    toText _ = T.pack "?"
    toBind = TE.decodeUtf8 . BS.Short.fromShort

instance ToText T.Text where
    toText _ = T.pack "?"
    toBind = id

instance ToText TL.Text where
    toText _ = T.pack "?"
    toBind = TL.toStrict

-- instance (Show a, Num a) => ToText a where
--     toBind _ = T.pack "?"
--     toText = T.pack . show

instance ToText Raw where
    toText (Raw t) = t
    toBind _ = T.pack ""

instance ToText Value where
    toText (Value t _) = t
    toBind (Value _ b) = b

