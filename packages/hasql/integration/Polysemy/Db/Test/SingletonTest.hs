module Polysemy.Db.Test.SingletonTest where

import Polysemy.Db.Data.Column (Auto, Prim)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import Polysemy.Db.Data.StoreError (StoreError)
import Polysemy.Db.Data.Table (Table(Table))
import qualified Polysemy.Db.Data.Uid as Uid
import Polysemy.Db.Database (interpretDatabase)
import Polysemy.Db.Schema.Generic (interpretSchemaSingleton)
import Polysemy.Db.Store (interpretStoreDb)
import Polysemy.Db.Table.Table (genTable)
import Polysemy.Test (UnitTest, assertRight)
import Polysemy.Db.Test.Database (withTestPlainTable)
import Polysemy.Db.Test.Run (integrationTest)

data Dat =
  Dat {
     id :: UUID,
     content :: Text
  }
  deriving (Eq, Show)

deriveGeneric ''Dat

data DatRep =
  DatRep {
    id :: Prim Auto,
    content :: Prim Auto
  }
  deriving (Eq, Show)

deriveGeneric ''DatRep

prog ::
  Member (Store () DbError Dat) r =>
  Sem r (Dat, (Either (StoreError DbError) (Maybe (NonEmpty Dat))))
prog = do
  let a = Dat id' "foo"
  Store.upsert a
  b <- Store.fetchAll
  pure (a, b)
  where
    id' = Uid.uuid 333

test_singletonDb :: UnitTest
test_singletonDb =
  integrationTest do
    (a, b) <- withTestPlainTable (genTable @Dat @DatRep) $ \table@(Table structure _ _) -> do
      interpretDatabase structure $
        interpretSchemaSingleton table $
        interpretStoreDb $
        prog
    assertRight (Just (pure a)) b
