{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE IncoherentInstances #-}

module QueryBuilder.ToText
    ( ToText(..)
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
    toBind :: a -> [T.Text]

instance ToText Char where
    toText = T.pack . show
    toBind _ = []

instance ToText String where
    toText = T.pack
    toBind _ = []

instance ToText B.Char8.ByteString where
    toText = TE.decodeUtf8
    toBind _ = []

instance ToText BL.Char8.ByteString where
    toText = TE.decodeUtf8 . B.concat . BL.toChunks
    toBind _ = []

instance ToText BS.Short.ShortByteString where
    toText = toText . BS.Short.fromShort
    toBind _ = []

instance ToText T.Text where
    toText = id
    toBind _ = []

instance ToText TL.Text where
    toText = TL.toStrict
    toBind _ = []

-- instance (Show a, Num a) => ToText a where
--     toText = T.pack . show
--     toBind _ = []

instance ToText Raw where
    toText _ = T.pack "?"
    toBind (Raw t) = [t]

