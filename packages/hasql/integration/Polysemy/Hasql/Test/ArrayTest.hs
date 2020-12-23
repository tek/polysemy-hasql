module Polysemy.Hasql.Test.ArrayTest where

import Polysemy.Db.Data.Column (Auto)
import Prelude hiding (Enum)

import Polysemy.Db.Data.Column (Enum, Prim)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.IdQuery (IdQuery(IdQuery), UuidQuery)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import qualified Polysemy.Db.Data.Uid as Uid
import Polysemy.Hasql.Test.Database (withTestStoreGen)
import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Test (UnitTest)
import Polysemy.Test.Hedgehog (assertJust)

data Flag =
  On
  |
  Off
  |
  Superposition
  deriving (Eq, Show, Generic)

data ArrayField =
  ArrayField {
    id :: UUID,
    f1 :: [Flag]
  }
  deriving (Eq, Show, Generic)

data ArrayFieldRep =
  ArrayFieldRep {
    id :: Prim,
    f1 :: Enum
  }
  deriving (Eq, Show, Generic)

id' :: UUID
id' =
  Uid.uuid 555

array :: ArrayField
array =
  ArrayField id' [On, Off, Superposition]

prog ::
  Member (Store UuidQuery ArrayField) r =>
  Sem r (Maybe ArrayField)
prog = do
  _ <- Store.upsert array
  Store.fetch (IdQuery id')

test_arrayField :: UnitTest
test_arrayField =
  integrationTest do
    result <- withTestStoreGen @Auto @ArrayFieldRep @UuidQuery @ArrayField (restop @DbError prog)
    assertJust array result
