module Polysemy.Hasql.Test.DefaultTest where

import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Test (UnitTest, assertEq)
import Prelude hiding (sum)
import Sqel (pgDefault)
import Sqel.Data.Dd (Dd, (:>) ((:>)))
import Sqel (QuerySchema)
import Sqel (TableSchema)
import Sqel (Uid (Uid))
import Sqel.PgType (tableSchema)
import Sqel (prim, primAs, primNullable)
import Sqel (prod)
import Sqel (checkQuery)
import Sqel.Uid (uid)

import Polysemy.Hasql.Interpreter.DbTable (interpretTable)
import Polysemy.Hasql.Interpreter.Store (interpretStoreDb)
import Polysemy.Hasql.Test.RunIntegration (integrationTest)

data Dat =
  Dat {
    name :: Text,
    number :: Int,
    count :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)

dd :: Dd _
dd = uid prim (prod (prim :> prim :> pgDefault "13" primNullable))

ts :: TableSchema (Uid Int Dat)
ts = tableSchema dd

idSchema :: QuerySchema Int (Uid Int Dat)
idSchema =
  checkQuery (primAs @"id") dd

-- TODO problem: default value is not used when explicit null is specified. would need to omit the column from the
-- insert statement, but that would require dynamic statement
test_default :: UnitTest
test_default =
  integrationTest do
    interpretTable ts $ interpretStoreDb ts idSchema do
      restop @DbError @(Store _ _) do
        Store.insert d
        assertEq [d] =<< Store.fetchAll
  where
    d = Uid 1 (Dat "name" 1 Nothing)
