{-# LANGUAGE OverloadedStrings #-}

module Spec.ToText
    ( toTextSpec
    ) where

import Test.Hspec

import Data.String
import qualified Data.ByteString.Char8      as BS.Char8
import qualified Data.ByteString.Lazy.Char8 as BS.Lazy.Char8
import qualified Data.ByteString.Short      as BS.Short

import qualified Data.ByteString.UTF8      as BS.UTF8
import qualified Data.ByteString.Lazy.UTF8 as BS.Lazy.UTF8

import qualified Data.Text      as T
import qualified Data.Text.Lazy as T.Lazy

import QueryBuilder.ToText
toTextSpec :: Spec
toTextSpec = do
  describe "conversion to Text" $ do

    context "to Text" $ do

      it "Char" $ do
        let expect = (T.pack . show) 'f'
            actual = toText ('f' :: Char)
        actual `shouldBe` expect

      it "String" $ do
        let expect = "hello"
            actual = toText ("hello" :: String)
        actual `shouldBe` expect

      it "ByteString" $ do
        let expect = "hello"
            actual = toText ("hello" :: BS.Char8.ByteString)
        actual `shouldBe` expect

      it "Lazy ByteString" $ do
        let expect = "hello"
            actual = toText ("hello" :: BS.Lazy.Char8.ByteString)
        actual `shouldBe` expect

      it "Short ByteString" $ do
        let expect = "hello"
            actual = toText ("hello" :: BS.Short.ShortByteString)
        actual `shouldBe` expect

      it "UTF8 ByteString" $ do
        let expect = "hello"
            actual = toText ("hello" :: BS.UTF8.ByteString)
        actual `shouldBe` expect

      it "UTF8 Lazy ByteString" $ do
        let expect = "hello"
            actual = toText ("hello" :: BS.Lazy.UTF8.ByteString)
        actual `shouldBe` expect

      it "Text" $ do
        let expect = "hello"
            actual = toText ("hello" :: T.Text)
        actual `shouldBe` expect

      it "Lazy Text" $ do
        let expect = "hello"
            actual = toText ("hello" :: T.Lazy.Text)
        actual `shouldBe` expect

