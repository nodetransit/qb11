import Test.Hspec
-- import qualified Spec.Condition as Condition
-- import qualified Spec.Column    as Column
import qualified Spec.Query     as Query

main :: IO ()
main = do
    putStrLn "QueryBuilder"
    hspec $ do
        -- Condition.runConditionSpec
        -- Column.runColumnSpec
        Query.runColumnSpec

