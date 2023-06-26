module Polysemy.Hasql.Test.ArrayTest where

import Data.Vector (Vector)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Test (UnitTest, assertJust)
import Prelude hiding (Enum)
import Sqel (Enum, Prim, Prod, Sqel, Uid (Uid), UidTable, query_Int, sqel)

import Polysemy.Hasql.Interpreter.DbTable (interpretTable)
import Polysemy.Hasql.Interpreter.Store (interpretStoreDb)
import Polysemy.Hasql.Test.RunIntegration (integrationTest)

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

type Table_ArrayField =
  UidTable "array_field" Int64 ArrayField Prim (Prod [Enum, Enum, Prim, Prim])

table_ArrayField :: Sqel Table_ArrayField
table_ArrayField = sqel

payload :: ArrayField
payload =
  ArrayField (Just [On, Off, Superposition]) [On, Off, Superposition] [1, 2, 3] [4, 5]

prog ::
  Member (Store Int64 ArrayField) r =>
  Sem r (Maybe ArrayField)
prog = do
  _ <- Store.upsert (Uid 555 payload)
  Store.fetchPayload 555

-- TODO add query with enum field
test_arrayField :: UnitTest
test_arrayField =
  integrationTest do
    interpretTable table_ArrayField $ interpretStoreDb query_Int table_ArrayField do
      result <- restop @DbError prog
      assertJust payload result
