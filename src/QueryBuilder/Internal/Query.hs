{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module QueryBuilder.Internal.Query
    ( Query(..)
    , QueryT(..)
    , Column(..)
    , Order(..)
    , Alias
    , defaultQuery
    ) where

import Data.Text as T
import Data.Text (Text)
import Control.Monad
import Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Applicative

import QueryBuilder
import QueryBuilder.Column
import QueryBuilder.Condition hiding (lift, liftIO)
import QueryBuilder.QueryOrder
import QueryBuilder.JoinTable


data Query = EmptyQuery
           | Select
           | Insert
           | Update
           | Delete
           | From Text
           | Table Text
        -- | TableAlias Text Alias
           | Into Text
           | Distinct
           | Columns [Column]
           | Values [[Text]]
           | GroupBy [Column]
           | Having QueryCondition
           | Join JoinType Text QueryCondition
        -- | InnerJoin Text QueryCondition
        -- | LeftJoin Text QueryCondition
        -- | RightJoin Text QueryCondition
        -- | OuterJoin Text QueryCondition
        -- | CrossJoin Text
           | JoinAlias JoinType Text Alias QueryCondition
        -- | InnerJoinAlias Text Alias QueryCondition
        -- | LeftJoinAlias Text Alias QueryCondition
        -- | RightJoinAlias Text Alias QueryCondition
        -- | OuterJoinAlias Text Alias QueryCondition
        -- | CrossJoinAlias Text Alias
           | Where QueryCondition
           | OrderBy [Column] Order
           | Limit Int
           | Comment [Text]
           | Query { query_type       :: Text
                   , query_table      :: Text
                   , query_distinct   :: Bool
                   , query_columns    :: [Column]
                   , query_values     :: QueryCondition
                   , query_groupBy    :: [Column]
                   , query_having     :: QueryCondition
                   , query_joins      :: [JoinTable]
                   , query_conditions :: QueryCondition
                   , query_orderBy    :: QueryOrder
                   , query_limit      :: Maybe Int
                   , query_comments   :: [Text]
               }
            deriving Show

-- | Default Query with empty values
defaultQuery = Query { query_type       = ""
                     , query_table      = ""
                     , query_distinct   = False
                     , query_columns    = []
                     , query_values     = mempty
                     , query_groupBy    = []
                     , query_having     = mempty
                     , query_joins      = []
                     , query_conditions = mempty
                     , query_orderBy    = QueryOrder[] None
                     , query_limit      = Nothing
                     , query_comments   = []
                     }

-- | Concatenate Queries
--
--   modify existing query if the property is empty
--   if the property is not empty, the current value
--   is not modified
modify_query :: Query -> Query -> Query
modify_query = mq
  where
    mq EmptyQuery         EmptyQuery        = EmptyQuery
    mq EmptyQuery         q                 = defaultQuery <> q
    mq q                  EmptyQuery        = defaultQuery <> q
    mq q@(Query {})       Select            = q { query_type = "SELECT" }
    mq q@(Query {})       Insert            = q { query_type = "INSERT" }
    mq q@(Query {})       Update            = q { query_type = "UPDATE" }
    mq q@(Query {})       Delete            = q { query_type = "DELETE" }
    mq q@(Query {})       (From t)          = q { query_table = t }
    mq q@(Query {})       (Table t)         = q { query_table = t }
    mq q@(Query {})       (Into t)          = q { query_table = t }
    mq q@(Query {})       (Columns c)       = q { query_columns = c }
    mq q@(Query {})       (Where c)         = q { query_conditions = c }
    mq q@(Query {})       (OrderBy c o)     = q { query_orderBy = QueryOrder c o }
    mq q@(Query {})       (GroupBy g)       = q { query_groupBy = g }
    mq q@(Query {})       (Having c)        = q { query_having = c }
    mq q@(Query {})       Distinct          = q { query_distinct = True }
    mq q@(Query {})       (Limit n)         = q { query_limit = Just n }
    mq q@(Query {})       (Values v)        = q { query_values = makeValues v }
    mq q@(Query {})       (Join u t c)      = q { query_joins = [makeJoinTable u t c] }
    mq q@(Query {})       (Comment t)       = q { query_comments = t }
    mq Select             q                 = defaultQuery { query_type = "SELECT" } <> q
    mq Insert             q                 = defaultQuery { query_type = "INSERT" } <> q
    mq Update             q                 = defaultQuery { query_type = "UPDATE" } <> q
    mq Delete             q                 = defaultQuery { query_type = "DELETE" } <> q
    mq (From t)           q                 = defaultQuery { query_table = t } <> q
    mq (Table t)          q                 = defaultQuery { query_table = t } <> q
    mq (Into t)           q                 = defaultQuery { query_table = t } <> q
    mq (Columns c)        q                 = defaultQuery { query_columns = c } <> q
    mq (Where c)          q                 = defaultQuery { query_conditions = c } <> q
    mq (OrderBy c o)      q                 = defaultQuery { query_orderBy = QueryOrder c o } <> q
    mq (GroupBy g)        q                 = defaultQuery { query_groupBy = g } <> q
    mq (Having c)         q                 = defaultQuery { query_having = c } <> q
    mq Distinct           q                 = defaultQuery { query_distinct = True } <> q
    mq (Limit n)          q                 = defaultQuery { query_limit = Just n } <> q
    mq (Values v)         q                 = defaultQuery { query_values = makeValues v } <> q
    mq (Join u t c)       q                 = defaultQuery { query_joins = [makeJoinTable u t c] } <> q
    mq (Comment t)        q                 = defaultQuery { query_comments = t }
    mq qL                 qR                = coalesceQuery qL qR

makeValues ll = rawQueryCondition clause values
  where
    values = Prelude.foldl (<>) [] ll
    join = T.intercalate ", "
    replacewith = Prelude.map (const "?")
    group q = "(" <> q <> ")"
    clause = join $ Prelude.map (group . join . replacewith) ll

makeJoinTable utype table cond = JoinTable
    { join_table      = table
    , join_type       = utype
    , join_alias      = mempty
    , join_conditions = cond
    }

-- | Merge Queries
coalesceQuery :: Query -> Query -> Query
coalesceQuery qL qR = Query { query_type       = queryType
                            , query_table      = queryTable
                            , query_distinct   = queryDistinct
                            , query_columns    = queryColumns
                            , query_values     = queryValues
                            , query_groupBy    = queryGroups
                            , query_having     = queryHaving
                            , query_joins      = queryJoins
                            , query_conditions = queryConditions
                            , query_orderBy    = queryOrder
                            , query_limit      = queryLimit
                            , query_comments   = queryComments
                            }
  where
    coalesce f g = if (f . g) qL /= 0 then g qL else g qR

    conditionLen = T.length . clause
    orderByLen = Prelude.length . columns
    distinctLen b = if b == True then 1 else 0
    limitLen Nothing = 0
    limitLen _       = 1

    queryType       = coalesce T.length       query_type
    queryTable      = coalesce T.length       query_table
    queryColumns    = coalesce Prelude.length query_columns
    queryConditions = coalesce conditionLen   query_conditions
    queryOrder      = coalesce orderByLen     query_orderBy
    queryGroups     = coalesce Prelude.length query_groupBy
    queryHaving     = coalesce conditionLen   query_having
    queryDistinct   = coalesce distinctLen    query_distinct
    queryLimit      = coalesce limitLen       query_limit
    queryValues     = coalesce conditionLen   query_values
    queryJoins      = query_joins qL <> query_joins qR
    queryComments   = coalesce Prelude.length query_comments

instance Semigroup Query where
    (<>) :: Query -> Query -> Query
    (<>) = modify_query

instance Monoid Query where
    mempty :: Query
    mempty = EmptyQuery

    mappend :: Query -> Query -> Query
    mappend = (<>)

data QueryT a m b = QueryT { runQueryT :: m (b, a) }

instance (Functor m) => Functor (QueryT a m) where
    fmap f = mapQueryT $ fmap $ \(a, w) -> (f a, w)
      where
        mapQueryT f m = QueryT $ f (runQueryT m)
    {-# INLINE fmap #-}

instance (Monoid a, Applicative m) => Applicative (QueryT a m) where
    pure a = QueryT $ pure (a, mempty)
    {-# INLINE pure #-}

    (<*>) f v = QueryT $ do
        liftA2 k f' v'
      where
        k (b, a) (b', a') = (b b', a <> a')
        f' = runQueryT f
        v' = runQueryT v
    {-# INLINE (<*>) #-}

-- instance (Monoid c, Alternative m) => Alternative (QueryT c m) where
--     empty = QueryT Control.Applicative.empty
--     {-# INLINE empty #-}
--
--     (<|>) m n = QueryT $ runQueryT m <|> runQueryT n
--     {-# INLINE (<|>) #-}

-- instance (Monoid w, MonadPlus m) => MonadPlus (QueryT w m) where
--     mzero = QueryT mzero
--     {-# INLINE mzero #-}
--
--     mplus left right = QueryT $ mplus (runQueryT left) (runQueryT right)
--     {-# INLINE mplus #-}

instance (Monoid a, Monad m) => Monad (QueryT a m) where
    return :: b -> QueryT a m b
    -- return b = QueryT $ \b -> return (b, mempty)
    return b = do
        (QueryT . return) (b, mempty)
    {-# INLINE return #-}

    -- (>>=) :: QueryT a m b -> (a -> QueryT a m b) -> QueryT a m b
    (>>=) m k = QueryT $ do
        (b, a)   <- runQueryT m
        (b', a') <- runQueryT (k b)
        return (b', a <> a')
    {-# INLINE (>>=) #-}

-- instance (Monoid c, Fail.MonadFail m) => Fail.MonadFail (QueryT c m) where
--     fail msg = QueryT $ Fail.fail msg
--     {-# INLINE fail #-}

instance (Monoid c, MonadIO m) => MonadIO (QueryT c m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance (Monoid c) => MonadTrans (QueryT c) where
    lift m = QueryT $ do
        a <- m
        return (a, mempty)
    {-# INLINE lift #-}
