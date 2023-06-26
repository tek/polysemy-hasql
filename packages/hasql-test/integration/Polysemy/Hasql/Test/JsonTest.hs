module Polysemy.Hasql.Test.JsonTest where

import qualified Data.Aeson as Aeson
import Exon (exon)
import qualified Hasql.Decoders as Decoders
import Hasql.Decoders (column, jsonBytes)
import qualified Hasql.Encoders as Encoders
import Hasql.Encoders (int8, param)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Test (UnitTest, assertJust, assertRight, evalMaybe)
import Sqel (Json, Prim, Prod, Sqel, Uid (Uid), UidTable, query_Int, sqel)

import qualified Polysemy.Hasql.Database as Database
import Polysemy.Hasql.Interpreter.DbTable (interpretTable)
import Polysemy.Hasql.Interpreter.Store (interpretStoreDb)
import Polysemy.Hasql.Test.RunIntegration (integrationTest)

data Field3 =
  Field3 {
    int :: Int,
    txt :: Text
  }
  deriving stock (Eq, Show, Generic)

json ''Field3

data Dat =
  Dat {
    field1 :: Text,
    field2 :: Int,
    field3 :: Field3
  }
  deriving stock (Eq, Show, Generic)

table_Dat :: Sqel (UidTable "dat" Int64 Dat Prim (Prod [Prim, Prim, Json]))
table_Dat = sqel

test_json :: UnitTest
test_json = do
  integrationTest do
    interpretTable table_Dat $ interpretStoreDb query_Int table_Dat do
      restop @DbError do
        Store.insert dat
        raise . assertJust dat =<< Store.fetch 5
    result <- evalMaybe =<< Database.retryingQuerySqlDef @DbError query dec enc 5
    assertRight f3 (Aeson.eitherDecodeStrict' result)
  where
    query = [exon|select field3 from dat where id = $1|]
    dec = column (Decoders.nonNullable (jsonBytes pure))
    enc = param (Encoders.nonNullable int8)
    dat = Uid 5 (Dat "field1" 8 f3)
    f3 = Field3 2 "field3"
