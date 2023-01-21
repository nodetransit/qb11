import Test.Hspec
-- import qualified Spec.Condition as Condition
import qualified Spec.Query     as Query
import qualified Spec.Column    as Column
import qualified Spec.Table     as Table

main :: IO ()
main = do
    putStrLn "QueryBuilder"
    hspec $ do
        Query.runColumnSpec
        Table.runTableSpec
        Column.runColumnSpec
        -- Condition.runConditionSpec

