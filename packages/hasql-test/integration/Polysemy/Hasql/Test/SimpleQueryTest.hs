module Polysemy.Hasql.Test.SimpleQueryTest where

import Lens.Micro.Extras (view)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Query as Query
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Test (UnitTest, (===))
import Sqel (Gen, IntTable, Query, Sqel, Uid (Uid), from, query_Int, select, sqel, where_)
import qualified Sqel.Syntax as Sqel
import Sqel.Syntax (query)

import Polysemy.Hasql.Effect.DbTable (DbTable)
import Polysemy.Hasql.Interpreter.DbTable (interpretTable)
import Polysemy.Hasql.Interpreter.Query (interpretQueryStatement)
import Polysemy.Hasql.Interpreter.Store (interpretStoreDb)
import Polysemy.Hasql.Test.RunIntegration (integrationTest)

data Q =
  Q {
    name :: Text,
    number :: Int64
  }
  deriving stock (Eq, Show, Generic)

type Query_Q = Query Q Gen

data Dat =
  Dat {
    name :: Text,
    number :: Int64,
    count :: Int64
  }
  deriving stock (Eq, Show, Generic)

type Table_Dat = IntTable "dat" Dat Gen

query_Q :: Sqel Query_Q
query_Q = sqel

table_Dat :: Sqel Table_Dat
table_Dat = sqel

interpretQuery ::
  Member (DbTable (Uid Int64 Dat) !! DbError) r =>
  InterpreterFor (Query.Query Q [Uid Int64 Dat] !! DbError) r
interpretQuery =
  interpretQueryStatement Sqel.do
    frags <- query query_Q table_Dat
    select frags.table
    from frags.table
    where_ frags.query

test_simpleQuery :: UnitTest
test_simpleQuery =
  integrationTest do
    interpretTable table_Dat $ interpretStoreDb query_Int table_Dat $ interpretQuery do
      restop @DbError @(Query.Query _ _) $ restop @DbError @(Store _ _) do
        for_ @[] [1..10] \ i ->
          Store.insert (Uid i (Dat "name" i 12))
        r <- fmap (view #id) <$> Query.query (Q "name" 5)
        [5] === r
