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
import QueryBuilder.ToString

data MyType =
    MyData1 Int String
  | MyData2 Double
  | MyData3 Char MyType
  deriving Show

toTextSpec :: Spec
toTextSpec = do
  describe "conversion to String/Text" $ do
    context "to String" $ do

      it "Int" $ do
        let expect = show 1818
            actual = toString (1818 :: Int)
        actual `shouldBe` expect

      it "Char" $ do
        let expect = show 'f'
            actual = toString ('f' :: Char)
        actual `shouldBe` expect

      it "Float" $ do
        let expect = show 5.28
            actual = toString (5.28 :: Float)
        actual `shouldBe` expect

      it "MyType" $ do
        let mydata1 = MyData1 3366 "sample text1"
        toString mydata1 `shouldBe` show mydata1

      it "String" $ do
        let expect = "hello"
            actual = toString ("hello" :: String)
        actual `shouldBe` expect

      it "ByteString" $ do
        let expect = "hello"
            actual = toString ("hello" :: BS.Char8.ByteString)
        actual `shouldBe` expect

      it "Lazy ByteString" $ do
        let expect = "hello"
            actual = toString ("hello" :: BS.Lazy.Char8.ByteString)
        actual `shouldBe` expect

      it "Short ByteString" $ do
        let expect = "hello"
            actual = toString ("hello" :: BS.Short.ShortByteString)
        actual `shouldBe` expect

      it "UTF8 ByteString" $ do
        let expect = "hello"
            actual = toString ("hello" :: BS.UTF8.ByteString)
        actual `shouldBe` expect

      it "UTF8 Lazy ByteString" $ do
        let expect = "hello"
            actual = toString ("hello" :: BS.Lazy.UTF8.ByteString)
        actual `shouldBe` expect

      it "Text" $ do
        let expect = "hello"
            actual = toString ("hello" :: T.Text)
        actual `shouldBe` expect

      it "Lazy Text" $ do
        let expect = "hello"
            actual = toString ("hello" :: T.Lazy.Text)
        actual `shouldBe` expect

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

