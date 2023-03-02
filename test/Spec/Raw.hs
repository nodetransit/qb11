{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}

module Spec.Raw
    ( rawSpec
    ) where


import Test.Hspec

import Data.Text (Text)
import QueryBuilder.ToText
import QueryBuilder.Raw
import Data.Semigroup

rawSpec :: Spec
rawSpec =
  describe "to text" $ do
    it "must be" $ do
       tquery f `shouldBe` "akane TEQ ?"
       tbindings f `shouldBe` ["love"]

data TQuery = TQuery
    { tquery    ::  Text
    , tbindings :: [Text]
    }
    deriving ( Show
             , Eq
             )

teq :: (ToText a, ToText b) => a -> b -> TQuery
teq l r = TQuery clause bindg
  where
    tl = toText l
    tr = toText r
    bl = toBind l
    br = toBind r

    clause :: Text
    clause = tl <> " TEQ " <> tr

    bindg  :: [Text]
    bindg  = bl <> br

f :: TQuery
f = "akane" `teq` ("love" :: Raw)


