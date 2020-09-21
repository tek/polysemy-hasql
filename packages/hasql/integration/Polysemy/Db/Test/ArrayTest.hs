module Polysemy.Db.Test.ArrayTest where

import Polysemy.Db.Data.Column (Auto, Prim)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Hasql.Data.Schema (IdQuery(IdQuery))
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import Polysemy.Db.Data.StoreError (StoreError)
import qualified Polysemy.Db.Data.Uid as Uid
import Polysemy.Hasql.Test.Database (withTestStoreGen)
import Polysemy.Db.Test.Run (integrationTest)
import Polysemy.Test (UnitTest, evalEither)
import Polysemy.Test.Hedgehog (assertJust)

data ArrayField =
  ArrayField {
    id :: UUID,
    f1 :: [Int]
  }
  deriving (Eq, Show, Generic)

data ArrayFieldRep =
  ArrayFieldRep {
    id :: Prim Auto,
    f1 :: Prim Auto
  }
  deriving (Eq, Show, Generic)

id' :: UUID
id' =
  Uid.uuid 555

array :: ArrayField
array =
  ArrayField id' [1, 2, 3]

prog ::
  Member (Store IdQuery DbError ArrayField) r =>
  Sem r (Either (StoreError DbError) (Maybe ArrayField))
prog = do
  _ <- Store.upsert array
  Store.fetch (IdQuery id')


test_arrayField :: UnitTest
test_arrayField =
  integrationTest do
    result <- withTestStoreGen @ArrayFieldRep prog
    assertJust array =<< evalEither result
