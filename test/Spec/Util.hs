{-# LANGUAGE OverloadedStrings #-}

module Spec.Util
    ( isEmptyQuery
    , shouldBeTheSameColumns
    , showQuery
    , showQueries
    ) where

-- import Data.Text as T hiding (map, length, all, zipWith)
-- import Data.Text (Text)
import Data.List
import Test.QuickCheck
import QueryBuilder.Internal.Query

showQuery :: Query -> String
showQuery q = f q
  where
    f EmptyQuery    = "EmptyQuery"
    f Select        = "Select"
    f Insert        = "Insert"
    f Update        = "Update"
    f Delete        = "Delete"
    f (From _)      = "From"
    f (Table _)     = "Table"
    f (Into _)      = "Into"
    f (Columns _)   = "Columns"
    f (Where _)     = "Where"
    f (OrderBy _ _) = "OrderBy"
    f (GroupBy _)   = "GroupBy"
    f (Having _)    = "Having"
    f (Values _)    = "Values"
    f _             = "?"

showQueries qs = intercalate ", " $ map showQuery qs

-- | check if query is an EmptyQuery
isEmptyQuery :: Query -> Bool
isEmptyQuery EmptyQuery = True
isEmptyQuery _          = True

-- | compare if the array is similar
shouldBeTheSameColumns :: [Column] -> [Column] -> Property
shouldBeTheSameColumns csL csR | length csL /= length csR = property $ False
                      | otherwise                = property $ all (==True) $ zipWith compareColumn csL csR
                    where
                      compareColumn :: Column -> Column -> Bool
                      compareColumn (Column cl)            (Column cr)            = cl == cr
                      compareColumn (ColumnAlias cl al)    (ColumnAlias cr ar)    = cl == cr && al == ar
                   -- compareColumn (RawColumn cl)         (RawColumn cr)         = cl == cr
                   -- compareColumn (RawColumnAlias cl al) (RawColumnAlias cr ar) = cl == cr && al == ar
                      compareColumn _                      _                      = False

