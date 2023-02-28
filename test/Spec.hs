{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}



import Test.Hspec
import Spec.Condition    as Run
import Spec.ConditionT   as Run
import Spec.Query        as Run
import Spec.QueryT       as Run
import Spec.Column       as Run
import Spec.Table        as Run
import Spec.ToText       as Run
import Spec.Operators    as Run
import Spec.RawOperators as Run
import Spec.Set          as Run

import Data.Text (Text)
import QueryBuilder.ToText
import Data.Semigroup

main :: IO ()
main = do
    putStrLn "QueryBuilder"
    hspec $ do
        runToText
        -- Run.querySpec
        -- Run.queryTSpec
        -- Run.tableSpec
        -- Run.columnSpec
        -- Run.toTextSpec
        -- Run.conditionSpec
        -- Run.conditionTSpec
        -- Run.operatorSpec
        -- Run.rawOperatorSpec
        -- Run.setSpec

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
    bindg  = [bl] <> [br]

runToText :: Spec
runToText =
  describe "to text" $ do
    it "must be" $ do
       tquery f `shouldBe` "akane"

f :: TQuery
f = "akane" `teq` ("love" :: Raw)

