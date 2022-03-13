module Polysemy.Hasql.Test.SingletonTest where

import Data.UUID (UUID)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Data.QueryStore as QueryStore
import Polysemy.Db.Data.QueryStore (QueryStore)
import Polysemy.Db.Data.Rep (Prim)
import qualified Polysemy.Db.Data.Uid as Uid
import Polysemy.Test (UnitTest, assertJust)

import Polysemy.Hasql.Crud (interpretCrudSingletonWith)
import Polysemy.Hasql.Interpreter.QueryStore (interpretQueryStoreDb)
import Polysemy.Hasql.ManagedTable (interpretManagedTable)
import Polysemy.Hasql.Table.BasicSchema (basicSchema)
import Polysemy.Hasql.Test.Database (withTestPlainTable)
import Polysemy.Hasql.Test.Run (integrationTest)

data Dat =
  Dat {
     id :: UUID,
     content :: Text
  }
  deriving stock (Eq, Show, Generic)

data DatRep =
  DatRep {
    id :: Prim,
    content :: Prim
  }
  deriving stock (Eq, Show, Generic)

prog ::
  Members [QueryStore () Dat () Dat !! DbError, Stop DbError] r =>
  Sem r (Dat, Maybe (NonEmpty Dat))
prog = do
  let a = Dat id' "foo"
  restop (QueryStore.upsert a)
  b <- restop QueryStore.fetchAll
  pure (a, b)
  where
    id' = Uid.uuid 333

test_singletonDb :: UnitTest
test_singletonDb =
  integrationTest do
    (a, b) <- withTestPlainTable (basicSchema @DatRep) $ \ table ->
      interpretManagedTable table $
        interpretCrudSingletonWith table $
        interpretQueryStoreDb $
        prog
    assertJust (pure a) b
