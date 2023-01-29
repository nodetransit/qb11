import Test.Hspec
import qualified Spec.Condition as Condition
import qualified Spec.Query     as Query
import qualified Spec.Column    as Column
import qualified Spec.Table     as Table
import qualified Spec.ToText    as ToText

main :: IO ()
main = do
    putStrLn "QueryBuilder"
    hspec $ do
        Query.runColumnSpec
        Table.runTableSpec
        Column.runColumnSpec
        Condition.runConditionSpec
        ToText.runToTextSpec

