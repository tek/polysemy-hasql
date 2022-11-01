{-# options_ghc -Wno-partial-type-signatures #-}

module Polysemy.Hasql.Test.Dsl.SimpleQueryTest where

import Lens.Micro.Extras (view)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import qualified Polysemy.Db.Effect.StoreQuery as StoreQuery
import Polysemy.Db.Effect.StoreQuery (StoreQuery (Basic))
import Polysemy.Test (UnitTest, (===))
import Prelude hiding (sum)
import Sqel.Data.Dd (Dd, (:>) ((:>)))
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Data.Uid (Uid (Uid))
import Sqel.PgType (tableSchema)
import Sqel.Prim (prim, primAs)
import Sqel.Product (prod, uid)
import Sqel.Query (checkQuery)
import Sqel.Statement (qStatement)

import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database)
import Polysemy.Hasql.Interpreter.Store (interpretManagedTable, interpretStoreDb)
import Polysemy.Hasql.Test.Run (integrationTest)

data Q =
  Q {
    name :: Text,
    number :: Int
  }
  deriving stock (Eq, Show, Generic)

data Dat =
  Dat {
    name :: Text,
    number :: Int,
    count :: Int
  }
  deriving stock (Eq, Show, Generic)

td :: Dd _
td = uid prim (prim :> prim :> prim)

ts :: TableSchema (Uid Int Dat)
ts = tableSchema td

idSchema :: QuerySchema Int (Uid Int Dat)
idSchema =
  checkQuery (primAs @"id") td

interpretQ ::
  Member (Database !! DbError) r =>
  InterpreterFor (StoreQuery Q [Uid Int Dat] !! DbError) r
interpretQ =
  interpretResumable \case
    Basic params ->
      restop (Database.statement params stm)
      where
        stm = qStatement (checkQuery (prod (prim :> prim)) td) ts

test_dslSimpleQuery :: UnitTest
test_dslSimpleQuery =
  integrationTest do
    interpretManagedTable ts $ interpretStoreDb ts idSchema $ interpretQ do
      restop @DbError @(StoreQuery _ _) $ restop @DbError @(Store _ _) do
        for_ @[] [1..10] \ i ->
          Store.insert (Uid i (Dat "name" i 12))
        r <- fmap (view #id) <$> StoreQuery.basic (Q "name" 5)
        [5] === r
