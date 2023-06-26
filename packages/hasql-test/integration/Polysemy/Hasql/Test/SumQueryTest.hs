module Polysemy.Hasql.Test.SumQueryTest where

import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Query as Query
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Test (UnitTest, (===))
import Sqel (Con1, Gen, Name, Prim, Query, Sqel, Sum, Uid (Uid), UidTable, query_Int, sqel)

import Polysemy.Hasql.Interpreter.DbTable (interpretTable)
import Polysemy.Hasql.Interpreter.Query (interpretQueryProj)
import Polysemy.Hasql.Interpreter.Store (interpretStoreDb)
import Polysemy.Hasql.Test.RunIntegration (integrationTest)

data Dat =
  Dat {
    name :: Text,
    number :: Int64
  }
  deriving stock (Eq, Show, Generic)

data NaNu =
  Na { name :: Text }
  |
  Nu Int64
  deriving stock (Eq, Show, Generic)

type Table_Dat =
  UidTable "dat" Int64 Dat Prim Gen

table_Dat :: Sqel Table_Dat
table_Dat = sqel

type Query_NaNu = Query NaNu (Sum [Gen, Con1 (Name "number" Prim)])

query_NaNu :: Sqel Query_NaNu
query_NaNu = sqel

test_sumQuery :: UnitTest
test_sumQuery =
  integrationTest $
  interpretTable table_Dat $
  interpretStoreDb query_Int table_Dat $
  interpretQueryProj @[_] query_NaNu table_Dat table_Dat.payload $
  restop @DbError @(Query.Query _ _) $
  restop @DbError @(Store _ _) do
    Store.insert (Uid 1 d1)
    Store.insert (Uid 2 d2)
    Store.insert (Uid 3 d3)
    r1 <- Query.query (Na "x")
    [d1, d2] === r1
    r2 <- Query.query (Nu 10)
    [d2, d3] === r2
  where
    d1 = Dat "x" 5
    d2 = Dat "x" 10
    d3 = Dat "y" 10
