{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}


module QueryBuilder.Internal.Condition
    ( ConditionT(..)
    , Condition(..)
    , condition
    , rawCondition
    , QueryCondition
    , equals
    , equalsRaw
    , notEquals
    , notEqualsRaw
    , greaterThan
    , greaterThanRaw
    , greaterThanOrEquals
    , greaterThanOrEqualsRaw
    , lessThan
    , lessThanRaw
    , lessThanOrEquals
    , lessThanOrEqualsRaw
    , is
    , isRaw
    , isNot
    , isNotRaw
    , not
    , notRaw
    , isIn
    , isInRaw
    , isNotIn
    , isNotInRaw
    , between
    , betweenRaw
    , notBetween
    , notBetweenRaw
    , isNull
    , isNotNull
    , like
    , likeRaw
    , notLike
    , notLikeRaw
    , (&&)
    , (&&...)
    , (||)
    , (||...)
    , and
    , or
    , null
    , true
    , false
    , begin
    ) where

import Data.Text as T hiding (null)
import Data.Text (Text)
import Data.Semigroup
import Control.Monad
import Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Applicative
import Prelude hiding (and, or, null, not, (&&), (||))

data
    -- (Monoid clause, Monoid bindings) =>
    Condition a b = Condition
        { clause   :: Text
        , bindings :: b
        }
        deriving ( Show
                 -- , Functor
                 , Eq)

type QueryCondition = Condition Text [Text]

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Semigroup (Condition a b) where
    (<>) (Condition aL bL) (Condition aR bR) = Condition (join aL aR) (bL <> bR)
      where
        join "" b  = b
        join a  "" = a
        join a  b  = a <> " " <> b

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Condition a b) where
    mempty  = Condition mempty mempty
    mappend = (<>)

-- instance Functor (Condition a) where
--     fmap :: (b1 -> b2) -> Condition a b1 -> Condition a b2
--     fmap f (Condition a b) = Condition a (f b)
-- 
-- instance Applicative (Condition a) where
--     pure = Condition (mempty, mempty)

-- instance Monad (Condition a) where
--     return a = Condition a' []
--       where
--         a' = "(" <> a <> ")"
-- 
--     -- (>>=) :: Condition a b -> (a -> Condition a b) -> Condition a b
--     -- (>>=) c f = Condition q' b'
--     --   where
--     --     c' = f $ clause c
--     --     q' = clause c'
--     --     b' = bindings c <> bindings c'

data ConditionT a m b = ConditionT { runConditionT :: m (b, a) }

instance (Functor m) => Functor (ConditionT a m) where
    fmap f = mapConditionT $ fmap $ \(a, w) -> (f a, w)
      where
        mapConditionT f m = ConditionT $ f (runConditionT m)
    {-# INLINE fmap #-}

instance (Semigroup a, Monoid a, Applicative m) => Applicative (ConditionT a m) where
    pure a = ConditionT $ pure (a, mempty)
    {-# INLINE pure #-}

    (<*>) f v = ConditionT $ do
        liftA2 k f' v'
      where
        k (b, a) (b', a') = (b b', a <> a')
        f' = runConditionT f
        v' = runConditionT v
    {-# INLINE (<*>) #-}

-- instance (Semigroup c, Monoid c, Alternative m) => Alternative (ConditionT c m) where
--     empty = ConditionT Control.Applicative.empty
--     {-# INLINE empty #-}
--
--     (<|>) m n = ConditionT $ runConditionT m <|> runConditionT n
--     {-# INLINE (<|>) #-}

-- instance (Semigroup w, Monoid w, MonadPlus m) => MonadPlus (ConditionT w m) where
--     mzero = ConditionT mzero
--     {-# INLINE mzero #-}
--
--     mplus left right = ConditionT $ mplus (runConditionT left) (runConditionT right)
--     {-# INLINE mplus #-}

instance (Semigroup a, Monoid a, Monad m) => Monad (ConditionT a m) where
    return :: b -> ConditionT a m b
    -- return b = ConditionT $ \b -> return (b, mempty)
    return b = do
        (ConditionT . return) (b, mempty)
    {-# INLINE return #-}

    -- (>>=) :: ConditionT a m b -> (a -> ConditionT a m b) -> ConditionT a m b
    (>>=) m k = ConditionT $ do
        (b, a)   <- runConditionT m
        (b', a') <- runConditionT (k b)
        return (b', a <> a')
    {-# INLINE (>>=) #-}

-- instance (Semigroup c, Monoid c, Fail.MonadFail m) => Fail.MonadFail (ConditionT c m) where
--     fail msg = ConditionT $ Fail.fail msg
--     {-# INLINE fail #-}

instance (Semigroup c, Monoid c, MonadIO m) => MonadIO (ConditionT c m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance (Semigroup c, Monoid c) => MonadTrans (ConditionT c) where
    lift m = ConditionT $ do
        a <- m
        return (a, mempty)
    {-# INLINE lift #-}

rawCondition :: Text -> QueryCondition
rawCondition c = Condition c []
{-# INLINABLE rawCondition #-}

condition :: Text -> QueryCondition -> QueryCondition
condition left right = Condition left [] <> right
{-# INLINABLE condition #-}

equals :: Text -> QueryCondition
equals v = Condition "= ?" [v]
{-# INLINABLE equals #-}

equalsRaw :: Text -> QueryCondition
equalsRaw v = Condition ("= " <> v) []
{-# INLINABLE equalsRaw #-}

notEquals :: Text -> QueryCondition
notEquals v = Condition "<> ?" [v]
{-# INLINABLE notEquals #-}

notEqualsRaw :: Text -> QueryCondition
notEqualsRaw v = Condition ("<> " <> v) []
{-# INLINABLE notEqualsRaw #-}

greaterThan :: Text -> QueryCondition
greaterThan v = Condition "> ?" [v]
{-# INLINABLE greaterThan #-}

greaterThanRaw :: Text -> QueryCondition
greaterThanRaw v = Condition ("> " <> v) []
{-# INLINABLE greaterThanRaw #-}

greaterThanOrEquals :: Text -> QueryCondition
greaterThanOrEquals v = Condition ">= ?" [v]
{-# INLINABLE greaterThanOrEquals #-}

greaterThanOrEqualsRaw :: Text -> QueryCondition
greaterThanOrEqualsRaw v = Condition (">= " <> v) []
{-# INLINABLE greaterThanOrEqualsRaw #-}

lessThan :: Text -> QueryCondition
lessThan v = Condition "< ?" [v]
{-# INLINABLE lessThan #-}

lessThanRaw :: Text -> QueryCondition
lessThanRaw v = Condition ("< " <> v) []
{-# INLINABLE lessThanRaw #-}

lessThanOrEquals :: Text -> QueryCondition
lessThanOrEquals v = Condition "<= ?" [v]
{-# INLINABLE lessThanOrEquals #-}

lessThanOrEqualsRaw :: Text -> QueryCondition
lessThanOrEqualsRaw v = Condition ("<= " <> v) []
{-# INLINABLE lessThanOrEqualsRaw #-}

isNull :: QueryCondition
isNull = Condition "IS NULL" []
{-# INLINABLE isNull #-}

isNotNull :: QueryCondition
isNotNull = Condition "IS NOT NULL" []
{-# INLINABLE isNotNull #-}

is :: Text -> QueryCondition
is v = Condition "IS ?" [v]
{-# INLINABLE is #-}

isRaw :: Text -> QueryCondition
isRaw v = Condition ("IS " <> v) []
{-# INLINABLE isRaw #-}

isNot :: Text -> QueryCondition
isNot v = Condition "IS NOT ?" [v]
{-# INLINABLE isNot #-}

isNotRaw :: Text -> QueryCondition
isNotRaw v = Condition ("IS NOT " <> v) []
{-# INLINABLE isNotRaw #-}

not :: Text -> QueryCondition
not v = Condition "NOT ?" [v]
{-# INLINABLE not #-}

notRaw :: Text -> QueryCondition
notRaw v = Condition ("NOT " <> v) []
{-# INLINABLE notRaw #-}

escapeList :: [Text] -> Text
escapeList = group . join . replacewith
  where
    group q = "(" <> q <> ")"
    join = T.intercalate ", "
    replacewith = Prelude.map (const "?")

isIn :: [Text] -> QueryCondition
isIn v = Condition ("IN " <> clause) v
  where
    clause = escapeList v
{-# INLINABLE isIn #-}

isInRaw :: [Text] -> QueryCondition
isInRaw v = Condition ("IN " <> clause) []
  where
    group q = "(" <> q <> ")"
    join = T.intercalate ", "
    clause = (group . join) v
{-# INLINABLE isInRaw #-}

isNotIn :: [Text] -> QueryCondition
isNotIn v = Condition ("NOT IN " <> clause) v
  where
    clause = escapeList v
{-# INLINABLE isNotInRaw #-}

isNotInRaw :: [Text] -> QueryCondition
isNotInRaw v = Condition ("NOT IN " <> clause) []
  where
    group q = "(" <> q <> ")"
    join = T.intercalate ", "
    clause = (group . join) v
{-# INLINABLE isNotIn #-}

between :: Text -> Text -> QueryCondition
between a c = Condition "BETWEEN ? AND ?" [a, c]
{-# INLINABLE between #-}

betweenRaw :: Text -> Text -> QueryCondition
betweenRaw a c = Condition ("BETWEEN " <> a <> " AND " <> c) []
{-# INLINABLE betweenRaw #-}

notBetween :: Text -> Text -> QueryCondition
notBetween a c = Condition "NOT BETWEEN ? AND ?" [a, c]
{-# INLINABLE notBetween #-}

notBetweenRaw :: Text -> Text -> QueryCondition
notBetweenRaw a c = Condition ("NOT BETWEEN " <> a <> " AND " <> c) []
{-# INLINABLE notBetweenRaw #-}

like :: Text -> QueryCondition
like v = Condition "LIKE ?" [v]
{-# INLINABLE like #-}

likeRaw :: Text -> QueryCondition
likeRaw v = Condition ("LIKE " <> v) []
{-# INLINABLE likeRaw #-}

notLike :: Text -> QueryCondition
notLike v = Condition "NOT LIKE ?" [v]
{-# INLINABLE notLike #-}

notLikeRaw :: Text -> QueryCondition
notLikeRaw v = Condition ("NOT LIKE " <> v) []
{-# INLINABLE notLikeRaw #-}

and :: QueryCondition
and = Condition "AND" []
{-# INLINABLE and #-}

or :: QueryCondition
or = Condition "OR" []
{-# INLINABLE or #-}

null :: Text
null = "NULL"
{-# INLINE null #-}

true :: Text
true = "1"
{-# INLINE true #-}

false :: Text
false = "0"
{-# INLINE false #-}

begin ::QueryCondition -> QueryCondition
begin c = Condition "(" [] <> c <> Condition ")" []
{-# INLINABLE begin #-}

(&&) :: QueryCondition -> QueryCondition -> QueryCondition
(&&) cL cR = cL <> and <> cR

(||) :: QueryCondition -> QueryCondition -> QueryCondition
(||) cL cR = cL <> or <> cR

(&&...) :: QueryCondition -> QueryCondition -> QueryCondition
(&&...) cL cR = cL <> and <> begin cR

(||...) :: QueryCondition -> QueryCondition -> QueryCondition
(||...) cL cR = cL <> or <> begin cR

