import Test.Hspec
import Spec.Condition    as Run
import Spec.ConditionT   as Run
import Spec.Query        as Run
import Spec.QueryT       as Run
import Spec.Column       as Run
import Spec.Alias        as Run
import Spec.Table        as Run
import Spec.ToText       as Run
import Spec.Operators    as Run
import Spec.RawOperators as Run
import Spec.Raw          as Run
import Spec.QueryBuilder as Run

import Spec.Examples.ExampleTest as Run

import Spec.Engines.Postgre as Run

import Data.Text (Text)
import QueryBuilder.ToText
import Data.Semigroup

main :: IO ()
main = do
    putStrLn "QueryBuilder"
    hspec $ do
        -- Run.querySpec
        Run.queryTSpec
        Run.tableSpec
        Run.columnSpec
        Run.aliasSpec
        Run.toTextSpec
        Run.conditionSpec
        Run.conditionTSpec
        Run.operatorSpec
        Run.rawOperatorSpec
        Run.rawSpec
        Run.queryBuilderSpec

        Run.selectExampleSpec

        Run.postgreSpec

