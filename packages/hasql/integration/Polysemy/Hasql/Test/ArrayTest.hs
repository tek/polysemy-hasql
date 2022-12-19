module Polysemy.Hasql.Test.ArrayTest where

import Data.UUID (UUID)
import Data.Vector (Vector)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (UuidStore)
import qualified Polysemy.Db.Store as Store
import Polysemy.Test (UnitTest)
import Polysemy.Test.Hedgehog (assertJust)
import Sqel.Column (nullable)
import Sqel.Data.Dd ((:>) ((:>)))
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.TableSchema (TableSchema)
import qualified Sqel.Data.Uid as Uid
import Sqel.Data.Uid (Uid (Uid), Uuid)
import Sqel.PgType (tableSchema)
import Sqel.Prim (array, enum, prim, primAs, readShow)
import Sqel.Product (prod)
import Sqel.Query (checkQuery)
import Sqel.Uid (uid)

import Polysemy.Hasql.Interpreter.Store (interpretDbTable, interpretStoreDb)
import Polysemy.Hasql.Test.Run (integrationTest)

data Flag = On | Off | Superposition
  deriving stock (Eq, Show, Generic, Ord, Read)

data ArrayField =
  ArrayField {
    f1 :: Maybe [Flag],
    f2 :: Set Flag,
    f3 :: Vector Int,
    f4 :: NonEmpty Double
  }
  deriving stock (Eq, Show, Generic)

table :: TableSchema (Uuid ArrayField)
query :: QuerySchema UUID (Uuid ArrayField)
(table, query) =
  (tableSchema dd, checkQuery (primAs @"id") dd)
  where
    dd = uid prim (prod (nullable (array enum) :> array readShow :> array prim :> array prim))

id' :: UUID
id' =
  Uid.intUUID 555

payload :: ArrayField
payload =
  ArrayField (Just [On, Off, Superposition]) [On, Off, Superposition] [1, 2, 3] [4, 5]

prog ::
  Member (UuidStore ArrayField) r =>
  Sem r (Maybe ArrayField)
prog = do
  _ <- Store.upsert (Uid id' payload)
  Store.fetchPayload id'

-- TODO add query with enum field
-- TODO add Maybe Array field
test_arrayField :: UnitTest
test_arrayField =
  integrationTest do
    interpretDbTable table $ interpretStoreDb table query do
      result <- restop @DbError prog
      assertJust payload result
