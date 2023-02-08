import Test.Hspec
import Spec.Condition  as Run
import Spec.ConditionT as Run
import Spec.Query      as Run
import Spec.Column     as Run
import Spec.Table      as Run
import Spec.ToText     as Run
import Spec.Operators  as Run

main :: IO ()
main = do
    putStrLn "QueryBuilder"
    hspec $ do
        Run.queryColumnSpec
        Run.tableSpec
        Run.columnSpec
        Run.toTextSpec
        Run.conditionSpec
        Run.conditionTSpec
        Run.operatorSpec

