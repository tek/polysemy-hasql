module Polysemy.Db.Test.PKTest where

import Polysemy.Db.Data.Column (Auto, Flatten, PK(PK), PKQuery(PKQuery), Prim, PrimaryKey)
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

data Rec =
  Rec {
    a :: Int,
    b :: Text
  }
  deriving (Eq, Show, Generic)

prog ::
  Member (Store (PKQuery Int) DbError (PK Int Rec)) r =>
  PK Int Rec ->
  Sem r (Either (StoreError DbError) (Maybe (PK Int Rec)))
prog specimen = do
  runError do
    Store.upsert specimen
    Store.fetch (PKQuery 2)

table :: QueryTable (PKQuery Int) (PK Int Rec)
table =
  queryTable

testRep ::
  Rep (PK Int Rec) ~ ProdTable [Prim PrimaryKey, Flatten (ProdColumn [Prim Auto, Prim Auto])] =>
  ()

testRep =
  ()

test_sumField :: UnitTest
test_sumField =
  integrationTest do
    _ <- pure testRep
    dbgs table
    result <- withTestStoreGen @Auto (prog specimen)
    assertJust specimen =<< evalEither result
  where
    specimen =
      PK 2 (Rec 5 "foo")
