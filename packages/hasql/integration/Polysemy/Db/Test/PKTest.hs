module Polysemy.Db.Test.PKTest where

import Polysemy.Db.Data.Column (Auto, Flatten, NewtypePrim, PK(PK), PKQuery(PKQuery), Prim, PrimaryKey)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.Store (Store)
import Polysemy.Db.Data.StoreError (StoreError)
import qualified Polysemy.Db.Store as Store
import Polysemy.Db.Test.Run (integrationTest)
import Polysemy.Hasql.Data.QueryTable (QueryTable)
import Polysemy.Hasql.Table.QueryTable (queryTable)
import Polysemy.Hasql.Table.Representation (ProdColumn, ProdTable, Rep)
import Polysemy.Hasql.Test.Database (withTestStoreGen)
import Polysemy.Test (UnitTest, evalEither)
import Polysemy.Test.Hedgehog (assertJust)

newtype Id =
  Id { unId :: Int }
  deriving (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

data Rec =
  Rec {
    a :: Int,
    b :: Text
  }
  deriving (Eq, Show, Generic)

prog ::
  Member (Store (PKQuery Id) DbError (PK NewtypePrim Id Rec)) r =>
  PK NewtypePrim Id Rec ->
  Sem r (Either (StoreError DbError) (Maybe (PK NewtypePrim Id Rec)))
prog specimen = do
  runError do
    Store.upsert specimen
    Store.fetch (PKQuery 2)

table :: QueryTable (PKQuery Id) (PK NewtypePrim Id Rec)
table =
  queryTable

testRep ::
  Rep (PK NewtypePrim Id Rec) ~ ProdTable [NewtypePrim PrimaryKey, Flatten (ProdColumn [Prim Auto, Prim Auto])] =>
  ()
testRep =
  ()

test_pk :: UnitTest
test_pk =
  integrationTest do
    _ <- pure testRep
    dbgs table
    result <- withTestStoreGen @Auto (prog specimen)
    assertJust specimen =<< evalEither result
  where
    specimen =
      PK 2 (Rec 5 "foo")
