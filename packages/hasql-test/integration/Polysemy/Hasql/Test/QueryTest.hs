module Polysemy.Hasql.Test.QueryTest where

import Lens.Micro.Extras (view)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Query as Query
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Test (UnitTest, (===))
import Sqel (
  Ignore,
  IntTable,
  Name,
  Newtype,
  OrNull,
  Param,
  Prim,
  Prod,
  Query,
  Sqel,
  Statement,
  Uid (Uid),
  from,
  limit,
  offset,
  query_Int,
  select,
  sqel,
  where_,
  )
import qualified Sqel.Syntax as Sqel
import Sqel.Syntax (query)

import Polysemy.Hasql.Interpreter.DbTable (interpretTable)
import Polysemy.Hasql.Interpreter.Query (interpretQueryProj, interpretQueryStatement)
import Polysemy.Hasql.Interpreter.Store (interpretStoreDb)
import Polysemy.Hasql.Test.RunIntegration (integrationTest)

newtype TextNt =
  TextNt { unTextNt :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

data PordQ =
  PordQ {
    p2 :: Text
  }
  deriving stock (Eq, Show, Generic)

data Par =
  Par {
    limit :: Maybe Int64,
    offset :: Maybe Int64
  }
  deriving stock (Eq, Show, Generic)

data Pord =
  Pord {
    p1 :: Int64,
    p2 :: Maybe TextNt
  }
  deriving stock (Eq, Show, Generic)

data Dat =
  Dat {
    name :: Text,
    po :: Pord
  }
  deriving stock (Eq, Show, Generic)

type Table_Dat = IntTable "dat" Dat (Prod [Prim, Prod [Prim, Newtype]])

table_Dat :: Sqel Table_Dat
table_Dat = sqel

data Q =
  Q {
    pr :: PordQ,
    params :: Par,
    n :: TextNt,
    ig :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)

type Query_Q =
  Query Q (Prod [
    Name "po" (Prod '[Prim]),
    Prod [Param (OrNull Prim), Param (OrNull Prim)],
    Name "name" Newtype,
    Ignore Prim
  ])

query_Q :: Sqel Query_Q
query_Q = sqel

statement :: Statement '[Uid Int64 Dat] Q (Uid Int64 Dat)
statement = Sqel.do
  frags <- query query_Q table_Dat
  select frags.table
  from frags.table
  where_ frags.query
  limit frags.query.params.limit
  offset frags.query.params.offset

inserts ::
  Member (Store Int64 Dat) r =>
  Sem r ()
inserts =
  for_ @[] [1..10] \ i ->
    Store.insert (Uid i (Dat "name" (Pord i (Just (TextNt "hnnnggg")))))

test_query :: UnitTest
test_query =
  integrationTest do
    interpretTable table_Dat $ interpretStoreDb query_Int table_Dat $ interpretQueryStatement @[_] statement do
      restop @DbError @(Query.Query _ _) $ restop @DbError @(Store _ _) do
        inserts
        r <- fmap (view #id) <$> Query.query (Q (PordQ "hnnnggg") (Par (Just 2) (Just 2)) "name" Nothing)
        [3, 4] === r
        r2 <- fmap (view #id) <$> Query.query (Q (PordQ "hnnnggg") (Par Nothing Nothing) "name" Nothing)
        10 === length r2

test_queryId :: UnitTest
test_queryId =
  integrationTest $
  interpretTable table_Dat $
  interpretStoreDb query_Int table_Dat $
  interpretQueryProj @[Int64] query_Int table_Dat table_Dat.id $
  restop @DbError @(Query.Query _ _) $
  restop @DbError @(Store _ _) do
    inserts
    r <- Query.query (5 :: Int64)
    [5] === r
