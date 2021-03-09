module Polysemy.Hasql.Test.SingletonTest where

import Polysemy.Db.Data.Column (Prim)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import qualified Polysemy.Db.Data.Uid as Uid
import Polysemy.Hasql.Data.Schema (Schema)
import Polysemy.Hasql.ManagedTable (interpretManagedTable)
import Polysemy.Hasql.Schema.Generic (interpretSchemaSingleton)
import Polysemy.Hasql.Store (interpretStoreDb)
import Polysemy.Hasql.Table.Table (genTable)
import Polysemy.Hasql.Test.Database (withTestPlainTable)
import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Test (UnitTest, assertJust)

data Dat =
  Dat {
     id :: UUID,
     content :: Text
  }
  deriving (Eq, Show, Generic)

data DatRep =
  DatRep {
    id :: Prim,
    content :: Prim
  }
  deriving (Eq, Show, Generic)

prog ::
  Members [Store () Dat !! DbError, Stop DbError] r =>
  Sem r (Dat, Maybe (NonEmpty Dat))
prog = do
  let a = Dat id' "foo"
  restop (Store.upsert a)
  b <- restop Store.fetchAll
  pure (a, b)
  where
    id' = Uid.uuid 333

test_singletonDb :: UnitTest
test_singletonDb =
  integrationTest do
    (a, b) <- withTestPlainTable (genTable @DatRep) $ \ table ->
      interpretManagedTable table $
        resumable @_ @(Schema _ _) (interpretSchemaSingleton table) $
        interpretStoreDb $
        prog
    assertJust (pure a) b
