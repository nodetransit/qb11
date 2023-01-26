module Spec.Condition
    ( runConditionSpec
    ) where

import Test.Hspec

runConditionSpec :: Spec
runConditionSpec =
    describe "akane" $
        do
            it "is love" $
                True `shouldBe` True
