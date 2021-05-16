module Polysemy.Hasql.Test.JsonTest where

import qualified Data.Aeson as Aeson
import qualified Hasql.Decoders as Decoders
import Hasql.Decoders (column, jsonBytes)
import qualified Hasql.Encoders as Encoders
import Hasql.Encoders (int8, param)
import Polysemy.Db.Data.Column (Json, Prim)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Uid (Uid(Uid))
import Polysemy.Test (UnitTest, assertRight, evalMaybe)

import qualified Polysemy.Hasql.Database as Database
import Polysemy.Hasql.Store (interpretStoreDbFullGenUid)
import Polysemy.Hasql.Test.Run (integrationTest)

data Field3 =
  Field3 {
    int :: Int,
    txt :: Text
  }
  deriving (Eq, Show, Generic)

defaultJson ''Field3

data Dat =
  Dat {
    field1 :: Text,
    field2 :: Int,
    field3 :: Field3
  }
  deriving (Eq, Show, Generic)

data DatRep =
  DatRep {
    field1 :: Prim,
    field2 :: Prim,
    field3 :: Json
  }
  deriving (Eq, Show, Generic)

test_json :: UnitTest
test_json =
  integrationTest do
    interpretStoreDbFullGenUid @DatRep @Prim @Int @Dat do
      restop @DbError $ Store.insert dat
    result <- evalMaybe =<< Database.retryingQuerySqlDef @DbError query enc dec 5
    assertRight f3 (Aeson.eitherDecodeStrict' result)
  where
    query =
      [text|select field3 from dat where id = $1|]
    enc =
      column (Decoders.nonNullable (jsonBytes pure))
    dec =
      param (Encoders.nonNullable int8)
    dat =
      Uid 5 (Dat "field1" 8 f3)
    f3 =
      Field3 2 "field3"
