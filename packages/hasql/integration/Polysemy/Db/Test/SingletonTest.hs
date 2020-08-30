module Polysemy.Db.Test.SingletonTest where

import Polysemy.Db.Data.Column (Auto, Prim)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import Polysemy.Db.Data.StoreError (StoreError)
import qualified Polysemy.Db.Data.Uid as Uid
import Polysemy.Db.Test.Run (integrationTest)
import Polysemy.Hasql.Data.Table (Table(Table))
import Polysemy.Hasql.Database (interpretDatabase)
import Polysemy.Hasql.Schema.Generic (interpretSchemaSingleton)
import Polysemy.Hasql.Store (interpretStoreDb)
import Polysemy.Hasql.Table.Table (genTable)
import Polysemy.Hasql.Test.Database (withTestPlainTable)
import Polysemy.Test (UnitTest, assertRight)

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
    (a, b) <- withTestPlainTable (genTable @DatRep) $ \table@(Table structure _ _) ->
      interpretDatabase structure $
        interpretSchemaSingleton table $
        interpretStoreDb $
        prog
    assertRight (Just (pure a)) b
