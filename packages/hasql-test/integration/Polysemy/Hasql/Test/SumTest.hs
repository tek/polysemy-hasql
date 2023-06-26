module Polysemy.Hasql.Test.SumTest where

import Lens.Micro.Extras (view)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Query as Query
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Test (UnitTest, (===))
import Sqel (Con, Gen, Name, Prim, PrimAs, Prod, Query, Sqel, Sum, TypeName, Uid (Uid), UidTable, query_Int, sqel)

import Polysemy.Hasql.Interpreter.DbTable (interpretTable)
import Polysemy.Hasql.Interpreter.Query (interpretQuery)
import Polysemy.Hasql.Interpreter.Store (interpretStoreDb)
import Polysemy.Hasql.Test.RunIntegration (integrationTest)

data Pord =
  Pord {
    p1 :: Int,
    p2 :: Text
  }
  deriving stock (Eq, Show, Generic)

data Sumbo =
  Glorpf { g1 :: Int, g2 :: Text }
  |
  Vnarp { v1 :: Int, v2 :: Text }
  |
  Shwank { s1 :: Text, s2 :: Pord }
  deriving stock (Eq, Show, Generic)

data Dat =
  Dat {
    name :: Text,
    sumb :: Sumbo
  }
  deriving stock (Eq, Show, Generic)

type Table_Dat = UidTable "dat" Int64 Dat Prim (TypeName "sombo" (Prod [Prim, Sum [Gen, Gen, Con [Prim, Gen]]]))

table_Dat :: Sqel Table_Dat
table_Dat = sqel

data SumboQ =
  GlorpfQ { g1 :: Int }
  |
  ShwankQ { s1 :: Text }
  deriving stock (Eq, Show, Generic)

data Q =
  Q {
    n :: Text,
    sumb :: SumboQ
  }
  deriving stock (Eq, Show, Generic)

type Query_Q =
  Query Q (Prod [
    PrimAs "name",
    TypeName "Sumbo" (Sum [Name "Glorpf" (Con '[Prim]), Name "Shwank" (Con '[Prim])])
  ])

query_Q :: Sqel Query_Q
query_Q = sqel

test_sum :: UnitTest
test_sum =
  integrationTest do
    interpretTable table_Dat $ interpretStoreDb query_Int table_Dat $ interpretQuery @[_] query_Q table_Dat do
      restop @DbError @(Query.Query _ _) $ restop @DbError @(Store _ _) do
        Store.insert (Uid 1 (Dat "ellow" (Glorpf 5 "crinp")))
        Store.insert (Uid 2 (Dat "ellow" (Glorpf 6 "crinp")))
        Store.insert (Uid 3 (Dat "cheerio" (Shwank "gzerq" (Pord 93 "pord"))))
        r1 <- fmap (view #id) <$> Query.query (Q "ellow" (GlorpfQ 5))
        [1] === r1
        r2 <- fmap (view #id) <$> Query.query (Q "cheerio" (ShwankQ "gzerq"))
        [3] === r2
