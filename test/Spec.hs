import Test.Hspec
import Spec.Condition as Run
import Spec.Query     as Run
import Spec.Column    as Run
import Spec.Table     as Run
import Spec.ToText    as Run

main :: IO ()
main = do
    putStrLn "QueryBuilder"
    hspec $ do
        Run.queryColumnSpec
        Run.tableSpec
        Run.columnSpec
        Run.conditionSpec
        Run.toTextSpec

