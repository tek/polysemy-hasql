module Polysemy.Hasql.Test.AnyTest where

import Polysemy.Db.Data.Rep (Auto, PrimaryKey, UidRep)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (UidStore)
import qualified Polysemy.Db.Data.StoreQuery as StoreQuery
import Polysemy.Db.Data.StoreQuery (StoreQuery)
import Polysemy.Db.Data.Uid (Uid(Uid))
import Polysemy.Test (Hedgehog, UnitTest, assert)

import Polysemy.Hasql.Query (interpretQuery)
import Polysemy.Hasql.Query.Any (interpretStoreQueryAny)
import Polysemy.Hasql.Test.Database (withTestStoreUid)
import Polysemy.Hasql.Test.Run (integrationTest)

data Dat =
  Dat {
    name :: Text
  }
  deriving (Eq, Show, Generic)

specimen :: Uid Int Dat
specimen =
  Uid 1 (Dat "name")

prog ::
  Members [UidStore Int Dat, StoreQuery Dat Bool, Hedgehog IO] r =>
  Sem r ()
prog = do
  Store.insert (Uid 1 (Dat "first"))
  assert =<< StoreQuery.basic (Dat "first")
  assert . not =<< StoreQuery.basic (Dat "second")

test_any :: UnitTest
test_any =
  integrationTest do
    withTestStoreUid $
      interpretQuery @Auto @(UidRep PrimaryKey Auto) $
      interpretStoreQueryAny @Dat @(Uid Int Dat) $
      restop @DbError @(StoreQuery Dat Bool) $ restop @DbError @(UidStore Int Dat) prog
