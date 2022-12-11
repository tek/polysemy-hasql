{-# options_ghc -Wno-partial-type-signatures #-}

module Polysemy.Hasql.Test.Dsl.DefaultTest where

import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Test (UnitTest, assertEq)
import Prelude hiding (sum)
import Sqel.Column (pgDefault)
import Sqel.Data.Dd (Dd, (:>) ((:>)))
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Data.Uid (Uid (Uid))
import Sqel.PgType (tableSchema)
import Sqel.Prim (prim, primAs, primNullable)
import Sqel.Product (uid)
import Sqel.Query (checkQuery)

import Polysemy.Hasql.Interpreter.Store (interpretDbTable, interpretStoreDb)
import Polysemy.Hasql.Test.Run (integrationTest)

data Dat =
  Dat {
    name :: Text,
    number :: Int,
    count :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)

td :: Dd _
td = uid prim (prim :> prim :> pgDefault "13" primNullable)

ts :: TableSchema (Uid Int Dat)
ts = tableSchema td

idSchema :: QuerySchema Int (Uid Int Dat)
idSchema =
  checkQuery (primAs @"id") td

-- TODO problem: default value is not used when explicit null is specified. would need to omit the column from the
-- insert statement, but that would require dynamic statement
test_dslSimpleQuery :: UnitTest
test_dslSimpleQuery =
  integrationTest do
    interpretDbTable ts $ interpretStoreDb ts idSchema do
      restop @DbError @(Store _ _) do
        Store.insert d
        assertEq [d] =<< Store.fetchAll
  where
    d = Uid 1 (Dat "name" 1 Nothing)
