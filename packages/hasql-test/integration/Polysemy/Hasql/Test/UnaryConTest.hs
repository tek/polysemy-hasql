module Polysemy.Hasql.Test.UnaryConTest where

import Lens.Micro.Extras (view)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Query as Query
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Test (UnitTest, (===))
import Sqel (Con1, Gen, Prim, PrimAs, Prod, Query, Sqel, Sum, Uid (Uid), UidTable, sqel)

import Polysemy.Hasql.Interpreter.DbTable (interpretTable)
import Polysemy.Hasql.Interpreter.Query (interpretQuery)
import Polysemy.Hasql.Interpreter.Store (interpretStoreDb)
import Polysemy.Hasql.Test.RunIntegration (integrationTest)

data F2 =
  F2 {
    a2 :: Text,
    b2 :: Int
  }
  deriving stock (Eq, Show, Generic)

data S =
  S1 { f1 :: Text }
  |
  S2 F2
  deriving stock (Eq, Show, Generic)

data Dat =
  Dat {
    name :: Text,
    s :: S
  }
  deriving stock (Eq, Show, Generic)

data Q =
  Q {
    name :: Text,
    s :: S
  }
  deriving stock (Eq, Show, Generic)

type Table_Dat = UidTable "dat" Int64 Dat Prim (Prod [Prim, Sum [Gen, Con1 (Prod [PrimAs "desc", Prim])]])

table_Dat :: Sqel Table_Dat
table_Dat = sqel

type Query_Int = Query Int64 (PrimAs "id")

query_Int :: Sqel Query_Int
query_Int = sqel

type Query_Q = Query Q (Prod [Prim, Sum [Gen, Con1 (Prod [PrimAs "desc", Prim])]])

query_Q :: Sqel Query_Q
query_Q = sqel

test_unaryCon :: UnitTest
test_unaryCon =
  integrationTest do
    interpretTable table_Dat $ interpretStoreDb query_Int table_Dat $ interpretQuery @[_] query_Q table_Dat do
      restop @DbError @(Query.Query _ _) $ restop @DbError @(Store _ _) do
        Store.insert (Uid 1 (Dat "ellow" (S1 "crinp")))
        Store.insert (Uid 2 (Dat "cheerio" (S2 (F2 "pord" 93))))
        r <- fmap (view #id) <$> Query.query (Q "ellow" (S1 "crinp"))
        [1] === r
