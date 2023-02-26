{-# options_ghc -Wno-partial-type-signatures -fconstraint-solver-iterations=10 #-}

module Polysemy.Hasql.Test.SumQueryTest where

import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Query as Query
import Polysemy.Db.Effect.Query (Query)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Test (UnitTest, (===))
import Prelude hiding (sum)
import Sqel.Data.Dd (Dd, DdK (DdK), Sqel, type (:>) ((:>)))
import Sqel.Data.Projection (Projection)
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Data.Uid (Uid (Uid))
import Sqel.PgType (projection, tableSchema)
import Sqel.Prim (prim, primAs, prims)
import Sqel.Product (prod)
import Sqel.Query (checkQuery)
import Sqel.Sum (con1, con1As, sum)
import Sqel.Uid (uid)

import Polysemy.Hasql.Interpreter.DbTable (interpretDbTable, interpretTableViewDd)
import Polysemy.Hasql.Interpreter.Query (interpretQuery)
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

td :: Sqel (Uid Int64 Dat) _
td = uid prim (prod prims)

ts :: TableSchema (Uid Int64 Dat)
ts = tableSchema td

vd :: Dd ('DdK _ _ Dat _)
vd = prod prims

vs :: Projection Dat (Uid Int64 Dat)
vs =
  projection vd td

idSchema :: QuerySchema Int64 (Uid Int64 Dat)
idSchema =
  checkQuery (primAs @"id") td

qd :: Dd ('DdK _ _ NaNu _)
qd = sum (con1 prim :> con1As @"number" prim)

test_sumQuery :: UnitTest
test_sumQuery =
  integrationTest do
    interpretDbTable ts $ interpretTableViewDd td vd $ interpretStoreDb ts idSchema $ interpretQuery @[_] vs (checkQuery qd td) do
      restop @DbError @(Query _ _) $ restop @DbError @(Store _ _) do
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
