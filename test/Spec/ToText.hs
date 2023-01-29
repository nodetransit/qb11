{-# LANGUAGE OverloadedStrings #-}

module Spec.ToText
    ( runToTextSpec
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

data MyType =
    MyData1 Int String
  | MyData2 Double
  | MyData3 Char MyType
  deriving Show

runToTextSpec :: Spec
runToTextSpec = do
  describe "toString" $ do

    it "Int => String" $ do
      let expect = show 1818
          actual = toString (1818 :: Int)
      actual `shouldBe` expect

    it "Char => String" $ do
      let expect = show 'f'
          actual = toString ('f' :: Char)
      actual `shouldBe` expect

    it "Float => String" $ do
      let expect = show 5.28
          actual = toString (5.28 :: Float)
      actual `shouldBe` expect

    it "MyType => String" $ do
      let mydata1 = MyData1 3366 "sample text1"
      toString mydata1 `shouldBe` show mydata1

    it "String => String" $ do
      let expect = "hello"
          actual = toString ("hello" :: String)
      actual `shouldBe` expect

    it "ByteString => String" $ do
      let expect = "hello"
          actual = toString ("hello" :: BS.Char8.ByteString)
      actual `shouldBe` expect

    it "Lazy ByteString => String" $ do
      let expect = "hello"
          actual = toString ("hello" :: BS.Lazy.Char8.ByteString)
      actual `shouldBe` expect

    it "Short ByteString => String" $ do
      let expect = "hello"
          actual = toString ("hello" :: BS.Short.ShortByteString)
      actual `shouldBe` expect

    it "UTF8 ByteString => String" $ do
      let expect = "hello"
          actual = toString ("hello" :: BS.UTF8.ByteString)
      actual `shouldBe` expect

    it "UTF8 Lazy ByteString => String" $ do
      let expect = "hello"
          actual = toString ("hello" :: BS.Lazy.UTF8.ByteString)
      actual `shouldBe` expect

    it "Text => String" $ do
      let expect = "hello"
          actual = toString ("hello" :: T.Text)
      actual `shouldBe` expect

    it "Lazy Text => String" $ do
      let expect = "hello"
          actual = toString ("hello" :: T.Lazy.Text)
      actual `shouldBe` expect

  describe "toText" $ do

    it "Int => Text" $ do
      let expect = (T.pack . show) 1818
          actual = toText (1818 :: Int)
      actual `shouldBe` expect

    it "Char => Text" $ do
      let expect = (T.pack . show) 'f'
          actual = toText ('f' :: Char)
      actual `shouldBe` expect

    it "Float => Text" $ do
      let expect = (T.pack . show) 5.28
          actual = toText (5.28 :: Float)
      actual `shouldBe` expect

    it "MyType => Text" $ do
      let mydata1 = MyData1 3366 "sample text1"
      toText mydata1 `shouldBe` (T.pack . show) mydata1

    it "String => Text" $ do
      let expect = "hello"
          actual = toText ("hello" :: String)
      actual `shouldBe` expect

    it "ByteString => Text" $ do
      let expect = "hello"
          actual = toText ("hello" :: BS.Char8.ByteString)
      actual `shouldBe` expect

    it "Lazy ByteString => Text" $ do
      let expect = "hello"
          actual = toText ("hello" :: BS.Lazy.Char8.ByteString)
      actual `shouldBe` expect

    it "Short ByteString => Text" $ do
      let expect = "hello"
          actual = toText ("hello" :: BS.Short.ShortByteString)
      actual `shouldBe` expect

    it "UTF8 ByteString => Text" $ do
      let expect = "hello"
          actual = toText ("hello" :: BS.UTF8.ByteString)
      actual `shouldBe` expect

    it "UTF8 Lazy ByteString => Text" $ do
      let expect = "hello"
          actual = toText ("hello" :: BS.Lazy.UTF8.ByteString)
      actual `shouldBe` expect

    it "Text => Text" $ do
      let expect = "hello"
          actual = toText ("hello" :: T.Text)
      actual `shouldBe` expect

    it "Lazy Text => Text" $ do
      let expect = "hello"
          actual = toText ("hello" :: T.Lazy.Text)
      actual `shouldBe` expect

