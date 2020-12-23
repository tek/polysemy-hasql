module Polysemy.Hasql.Test.UpsertTest where

import Polysemy.Test (UnitTest, assertJust)

import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (UidStore)
import Polysemy.Db.Data.Uid (Uid(Uid))
import Polysemy.Hasql.Test.Database (withTestStoreUid)
import Polysemy.Hasql.Test.Run (integrationTest)

data Dat =
  Dat {
    name :: Text
  }
  deriving (Eq, Show, Generic)

specimen :: Uid Int Dat
specimen =
  Uid 1 (Dat "second")

prog ::
  Member (UidStore Int Dat) r =>
  Sem r (Maybe (NonEmpty (Uid Int Dat)))
prog = do
  Store.insert (Uid 1 (Dat "first"))
  Store.upsert specimen
  Store.fetchAll

test_upsert :: UnitTest
test_upsert = do
  integrationTest do
    withTestStoreUid @Int @Dat do
      assertJust [specimen] =<< restop @DbError prog
