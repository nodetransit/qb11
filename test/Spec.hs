import Test.Hspec
-- import qualified Spec.Condition as Condition
import qualified Spec.Query     as Query
import qualified Spec.Column    as Column

main :: IO ()
main = do
    putStrLn "QueryBuilder"
    hspec $ do
        Query.runColumnSpec
        Column.runColumnSpec
        -- Condition.runConditionSpec

