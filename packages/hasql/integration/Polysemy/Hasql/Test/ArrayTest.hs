module Polysemy.Hasql.Test.ArrayTest where

-- import Data.UUID (UUID)
-- import Polysemy.Db.Data.DbError (DbError)
-- import qualified Polysemy.Db.Effect.Store as Store
-- import Polysemy.Db.Effect.Store (UuidStore)
-- import qualified Polysemy.Db.Store as Store
-- import Polysemy.Test (UnitTest)
-- import Polysemy.Test.Hedgehog (assertJust)
-- import Prelude hiding (Enum)
-- import Sqel.Combinators (primAs)
-- import Sqel.Data.QuerySchema (QuerySchema)
-- import Sqel.Data.TableSchema (TableSchema)
-- import qualified Sqel.Data.Uid as Uid
-- import Sqel.Data.Uid (Uid (Uid), Uuid)
-- import Sqel.PgType (tableSchema)
-- import Sqel.Prim (prims)
-- import Sqel.Product (prod)
-- import Sqel.Query (checkQuery)

-- import Polysemy.Hasql.Interpreter.Store (interpretDbTable, interpretStoreDb)
-- import Polysemy.Hasql.Test.Run (integrationTest)

-- data Flag =
--   On
--   |
--   Off
--   |
--   Superposition
--   deriving stock (Eq, Show, Generic, Ord)

-- data ArrayField =
--   ArrayField {
--     f1 :: [Flag],
--     f2 :: Set Flag
--   }
--   deriving stock (Eq, Show, Generic)

-- id' :: UUID
-- id' =
--   Uid.intUUID 555

-- array :: ArrayField
-- array =
--   ArrayField [On, Off, Superposition] [On, Off, Superposition]

-- prog ::
--   Member (UuidStore ArrayField) r =>
--   Sem r (Maybe ArrayField)
-- prog = do
--   _ <- Store.upsert (Uid id' array)
--   Store.fetchPayload id'

-- table :: TableSchema (Uuid ArrayField)
-- query :: QuerySchema UUID (Uuid ArrayField)
-- (table, query) =
--   (tableSchema dd, checkQuery (primAs @"id") dd)
--   where
--     dd = prod prims

-- test_arrayField :: UnitTest
-- test_arrayField =
--   integrationTest do
--     interpretDbTable table $ interpretStoreDb table query do
--       result <- restop @DbError prog
--       assertJust array result
