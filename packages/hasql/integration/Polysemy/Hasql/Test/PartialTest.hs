module Polysemy.Hasql.Test.PartialTest where

import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.PartialFields (PartialFields)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (UidStore)
import Polysemy.Db.Data.Uid (Uid(Uid))
import Polysemy.Db.Partial (field, partial, (.>))
import Polysemy.Test (UnitTest, assertJust)

import Polysemy.Hasql.Test.Database (withTestStoreUid)
import Polysemy.Hasql.Test.Run (integrationTest)

data Dat =
  Dat {
    int :: Int,
    double :: Double,
    texts :: [Text]
  }
  deriving (Eq, Show, Generic)

partialUpdate :: PartialFields Dat
partialUpdate =
  partial @Dat .> field @"int" 5 .> field @"double" 17.5

record :: Uid Int Dat
record =
  Uid 1 (Dat 9 5 ["hello"])

target :: Uid Int Dat
target =
  Uid 1 (Dat 5 17.5 ["hello"])

prog ::
  Member (UidStore Int Dat) r =>
  Sem r (Maybe (NonEmpty (Uid Int Dat)))
prog = do
  Store.insert record
  -- Store.update 1 partialUpdate
  Store.fetchAll

test_partialDbUpdate :: UnitTest
test_partialDbUpdate =
  integrationTest do
    withTestStoreUid @Int @Dat do
      assertJust [target] =<< restop @DbError prog
