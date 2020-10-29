module Polysemy.Hasql.Test.UpsertTest where

import Polysemy.Test (UnitTest)

import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.Store (UidStore)
import Polysemy.Db.Data.StoreError (StoreError)
import Polysemy.Db.Data.Uid (Uid(Uid))
import qualified Polysemy.Db.Store as Store
import Polysemy.Hasql.Test.Database (withTestStoreUid)
import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Test (assertJust, evalEither)

data Dat =
  Dat {
    name :: Text
  }
  deriving (Eq, Show, Generic)

specimen :: Uid Int Dat
specimen =
  Uid 1 (Dat "second")

prog ::
  Members [Error (StoreError DbError), UidStore Int DbError Dat] r =>
  Sem r (Maybe (NonEmpty (Uid Int Dat)))
prog = do
  Store.insert (Uid 1 (Dat "first"))
  Store.upsert specimen
  Store.fetchAll


test_upsert :: UnitTest
test_upsert = do
  integrationTest do
    withTestStoreUid do
      assertJust [specimen] =<< evalEither =<< runError prog
