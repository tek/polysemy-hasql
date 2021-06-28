module Polysemy.Hasql.Test.ArrayTest where

import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.Rep (Enum)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (UuidStore)
import qualified Polysemy.Db.Data.Uid as Uid
import Polysemy.Db.Data.Uid (Uid (Uid))
import qualified Polysemy.Db.Store as Store
import Polysemy.Test (UnitTest)
import Polysemy.Test.Hedgehog (assertJust)
import Prelude hiding (Enum)

import Polysemy.Hasql.Test.Database (withTestStoreGen)
import Polysemy.Hasql.Test.Run (integrationTest)

data Flag =
  On
  |
  Off
  |
  Superposition
  deriving (Eq, Show, Generic)

data ArrayField =
  ArrayField {
    f1 :: [Flag]
  }
  deriving (Eq, Show, Generic)

data ArrayFieldRep =
  ArrayFieldRep {
    f1 :: Enum
  }
  deriving (Eq, Show, Generic)

id' :: UUID
id' =
  Uid.uuid 555

array :: ArrayField
array =
  ArrayField [On, Off, Superposition]

prog ::
  Member (UuidStore ArrayField) r =>
  Sem r (Maybe ArrayField)
prog = do
  _ <- Store.upsert (Uid id' array)
  Store.fetchPayload id'

test_arrayField :: UnitTest
test_arrayField =
  integrationTest do
    result <- withTestStoreGen @ArrayFieldRep (restop @DbError prog)
    assertJust array result
