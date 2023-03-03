{-# LANGUAGE OverloadedStrings #-}

module Spec.ConditionT
    ( conditionTSpec
    ) where

import Prelude hiding (and, or, null, (&&), (||))
import Data.Text as T hiding (null)
import Data.Semigroup
import Test.Hspec
import Control.Monad
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import System.IO.Unsafe

import QueryBuilder.Condition
import QueryBuilder.Condition.Operators

conditionTSpec :: Spec
conditionTSpec =
    describe "condition transformer" $ do
      context "simple query" $ do
        it "using identity monad" $ do
          let q = testConditionTransformer
          clause q `shouldBe` "a = ? AND b IS NULL AND ( c <> ? OR c IS NOT NULL OR ( d <> ? AND e IS NOT ? ) ) AND g LIKE ?"
          bindings q `shouldBe` ["1", "", "0", "F", "%G%"]

        it "using identity monad and andBegin" $ do
          let q = testConditionTransformerAndBegin
          clause q `shouldBe` "a = ? AND ( c <> ? OR c IS NOT NULL OR ( d <> ? AND e IS NOT ? ) ) AND g LIKE ?"
          bindings q `shouldBe` ["1", "", "0", "F", "%G%"]

        it "using maybe monad" $ do
          clause testConditionTransformerMaybe `shouldBe` "a = ? AND b IS NULL"
          bindings testConditionTransformerMaybe `shouldBe` ["A"]

        it "using implicit identity monad" $ do
          clause testCondition `shouldBe` "a = ? AND b IS NULL AND ( c <> ? OR c IS NOT NULL )"
          bindings testCondition `shouldBe` ["1", ""]

        -- it "alternative" $ do
        --   clause testConditionTransformerAlternative `shouldBe` "x = ?"

        -- it "monad plus" $ do
        --   "NOT IMPLEMENTED" `shouldNotBe` "NOT IMPLEMENTED"

        -- it "fail" $ do
        --   "NOT IMPLEMENTED" `shouldNotBe` "NOT IMPLEMENTED"

        it "lift IO" $ do
          clause testConditionTransformerIO `shouldBe` "a <> ?"
          bindings testConditionTransformerIO `shouldBe` ["B"]

        it "using operators" $ do
          clause testConditionTransformerOperators `shouldBe` "a = ? AND b IS NULL AND ( c <> ? OR c IS NOT NULL OR ( d IS NOT ? AND f IS NOT ? ) ) AND h LIKE ?"
          bindings testConditionTransformerOperators `shouldBe` ["1", "", "0", "g", "%H%"]

        it "raw queries" $ do
          clause testRawConditionT `shouldBe` "a = true AND b is NULL"
          bindings testRawConditionT `shouldBe` []

testCondition:: QueryCondition
testCondition= (runConditionM) createQueryCondition
  where
    createQueryCondition :: ConditionM
    createQueryCondition = do
        condition "a" (equals true)
        and "b" isNull
        and `begin` do
            condition "c" (notEquals "")
            or "c" isNotNull

testConditionTransformer :: QueryCondition
testConditionTransformer = (runIdentity .runConditionT) createQueryCondition
  where
    createQueryCondition :: ConditionT Identity
    createQueryCondition = do
        condition "a" (equals true)
        and "b" isNull
        and `begin` do
            condition "c" (notEquals "")
            or "c" isNotNull
            or `begin` do
                condition "d" (notEquals false)
                and "e" (isNot "F")
        and "g" (like "%G%")

testConditionTransformerAndBegin :: QueryCondition
testConditionTransformerAndBegin = (runIdentity .runConditionT) createQueryCondition
  where
    createQueryCondition :: ConditionT Identity
    createQueryCondition = do
        condition "a" (equals true)
        and_ $ do
            condition "c" (notEquals "")
            or "c" isNotNull
            or_ $ do
                condition "d" (notEquals false)
                and "e" (isNot "F")
        and "g" (like "%G%")

testConditionTransformerMaybe :: QueryCondition
testConditionTransformerMaybe = (runMaybe . runConditionT) createMaybeQueryCondition
  where
    runMaybe :: (Monoid a) => Maybe a -> a
    runMaybe (Just x) = x
    runMaybe _        = mempty

    createMaybeQueryCondition :: ConditionT Maybe
    createMaybeQueryCondition = do
        input <- lift getInput
        condition "a" (equals input)
        and "b" isNull

    getInput :: Maybe Text
    getInput = Just "A"

testConditionTransformerOperators :: QueryCondition
testConditionTransformerOperators = (runIdentity .runConditionT) createQueryCondition
  where
    createQueryCondition :: ConditionT Identity
    createQueryCondition = do
           condition "a" (equals true)
        && condition "b" isNull
        &&... ( do
            condition "c" (notEquals "")
            || condition "c" isNotNull
            ||... ( do
                condition "d" (isNot false)
                && condition "f" (isNot "g")
            )
        )
        && condition "h" (like "%H%")

testConditionTransformerIO :: QueryCondition
testConditionTransformerIO = (unsafePerformIO . runConditionT) createMaybeQueryCondition
  where
    createMaybeQueryCondition :: ConditionT IO
    createMaybeQueryCondition = do
        input <- liftIO getInput
        condition "a" (notEquals input)

    getInput :: IO (Text)
    getInput = return "B"

testRawConditionT :: QueryCondition
testRawConditionT = (runIdentity . runConditionT) createRawCondition
  where
    createRawCondition = do
        raw "a = true"
        && raw "b is NULL"

