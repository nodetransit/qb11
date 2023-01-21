module Spec.Util
    ( isEmptyQuery
    , isSameColumns
    ) where

import QueryBuilder.Query

-- | check if query is an EmptyQuery
isEmptyQuery :: Query -> Bool
isEmptyQuery EmptyQuery = True
isEmptyQuery _          = True

-- | compare if the array is similar
isSameColumns :: [Column] -> [Column] -> Bool
isSameColumns csL csR | length csL /= length csR = False
                      | otherwise = all (==True) $ zipWith compareColumn csL csR
                    where
                      compareColumn :: Column -> Column -> Bool
                      compareColumn (Column cl)            (Column cr)            = cl == cr
                      compareColumn (ColumnAlias cl al)    (ColumnAlias cr ar)    = cl == cr && al == ar
                      compareColumn (RawColumn cl)         (RawColumn cr)         = cl == cr
                      compareColumn (RawColumnAlias cl al) (RawColumnAlias cr ar) = cl == cr && al == ar
                      compareColumn _                   _                         = False

