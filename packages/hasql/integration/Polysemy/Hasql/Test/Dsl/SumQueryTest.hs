{-# options_ghc -Wno-partial-type-signatures -fconstraint-solver-iterations=10 #-}

module Polysemy.Hasql.Test.Dsl.SumQueryTest where

import Hasql.Statement (Statement)
import Lens.Micro.Extras (view)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Query as Query
import Polysemy.Db.Effect.Query (Query (Query))
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Test (UnitTest, (===))
import Prelude hiding (sum)
import Sqel.Data.Dd (Dd, DdK (DdK), type (:>) ((:>)))
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Data.Uid (Uid (Uid))
import Sqel.PgType (tableSchema)
import Sqel.Prim (prim, primAs, prims)
import Sqel.Product (uid)
import Sqel.Query (checkQuery)
import Sqel.Statement (selectWhere)
import Sqel.Sum (con1, con1As, sum)

import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database)
import Polysemy.Hasql.Interpreter.Store (interpretDbTable, interpretStoreDb)
import Polysemy.Hasql.Test.Run (integrationTest)

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

td :: Dd ('DdK _ _ (Uid Int64 Dat) _)
td =
  uid prim prims

ts :: TableSchema (Uid Int64 Dat)
ts = tableSchema td

idSchema :: QuerySchema Int64 (Uid Int64 Dat)
idSchema =
  checkQuery (primAs @"id") td

stm :: Statement NaNu [Uid Int64 Dat]
stm =
  selectWhere (checkQuery qd td) (tableSchema td)
  where
    qd = sum (con1 prim :> con1As @"number" prim)

interpretQuery ::
  Member (Database !! DbError) r =>
  InterpreterFor (Query NaNu [Uid Int64 Dat] !! DbError) r
interpretQuery =
  interpretResumable \case
    Query params ->
      restop (Database.statement params stm)

test_dslSumQuery :: UnitTest
test_dslSumQuery =
  integrationTest do
    interpretDbTable ts $ interpretStoreDb ts idSchema $ interpretQuery do
      restop @DbError @(Query _ _) $ restop @DbError @(Store _ _) do
        Store.insert (Uid 1 (Dat "x" 5))
        Store.insert (Uid 2 (Dat "y" 10))
        Store.insert (Uid 3 (Dat "x" 10))
        r1 <- fmap (view #id) <$> Query.query (Na "x")
        [1, 3] === r1
        r2 <- fmap (view #id) <$> Query.query (Nu 10)
        [2, 3] === r2
