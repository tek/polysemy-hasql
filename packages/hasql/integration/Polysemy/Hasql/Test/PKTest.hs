module Polysemy.Hasql.Test.PKTest where

import Polysemy.Db.Data.Column (Auto, Flatten, NewtypePrim, Prim, PrimaryKey)
import Polysemy.Db.Data.ColumnOptions (primaryKey)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.Store (Store)
import Polysemy.Db.Data.StoreError (StoreError)
import Polysemy.Db.Data.TableStructure (Column(Column), TableStructure(TableStructure))
import Polysemy.Db.Data.Uid (Uid(Uid))
import qualified Polysemy.Db.Store as Store
import Polysemy.Hasql.Data.QueryTable (QueryTable)
import Polysemy.Hasql.Data.Schema (IdQuery(IdQuery))
import Polysemy.Hasql.Table.QueryTable (queryTable)
import Polysemy.Hasql.Table.Representation (ProdColumn, ProdTable, Rep)
import Polysemy.Hasql.Table.TableStructure (tableStructure)
import Polysemy.Hasql.Test.Database (withTestStoreGen)
import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Test (UnitTest, assertJust, evalEither, (===))

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
  Member (Store (IdQuery Id) DbError (Uid Id Rec)) r =>
  Uid Id Rec ->
  Sem r (Either (StoreError DbError) (Maybe (Uid Id Rec)))
prog specimen = do
  runError do
    Store.upsert specimen
    Store.fetch (IdQuery 2)

struct :: TableStructure
struct =
  tableStructure @(Uid Id Rec)

table :: QueryTable (IdQuery Id) (Uid Id Rec)
table =
  queryTable

testRep ::
  Rep (Uid Id Rec) ~ ProdTable [NewtypePrim PrimaryKey, Flatten (ProdColumn [Prim Auto, Prim Auto])] =>
  ()
testRep =
  ()

targetStructure :: TableStructure
targetStructure =
  TableStructure "rec" [
    Column "id" "bigint" def { primaryKey = True } Nothing,
    Column "a" "bigint" def Nothing,
    Column "b" "text" def Nothing
  ]

test_pk :: UnitTest
test_pk =
  integrationTest do
    _ <- pure testRep
    targetStructure === struct
    result <- withTestStoreGen @Auto (prog specimen)
    assertJust specimen =<< evalEither result
  where
    specimen =
      Uid 2 (Rec 5 "foo")
