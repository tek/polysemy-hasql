module Polysemy.Db.Test.SumFieldTest where

import Generics.SOP.Type.Metadata (ConstructorInfo(Record), FieldInfo(FieldInfo))

import Polysemy.Db.Data.Column (Auto, Prim, Sum)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.Store (Store)
import Polysemy.Db.Data.StoreError (StoreError)
import Polysemy.Db.Data.TableStructure (Column, CompositeType, TableStructure)
import qualified Polysemy.Db.Data.Uid as Uid
import qualified Polysemy.Db.Store as Store
import Polysemy.Db.Test.Run (integrationTest)
import Polysemy.Hasql.Data.QueryTable (QueryTable)
import Polysemy.Hasql.Data.Schema (IdQuery(IdQuery))
import Polysemy.Hasql.Data.Table (Table)
import Polysemy.Hasql.Table.Columns (genCompositeType, genCtorType, genSumColumns, genSumCtorsColumns, genColumns')
import Polysemy.Hasql.Table.QueryTable (genQueryTable)
import Polysemy.Hasql.Table.Table (genTable)
import Polysemy.Hasql.Table.TableStructure (genTableStructure)
import Polysemy.Hasql.Test.Database (withTestStoreGen)
import Polysemy.Test (UnitTest, evalEither)
import Polysemy.Test.Hedgehog (assertJust)

data Summy =
  Laevus { lInt :: Int, lText :: Text }
  |
  Dexter { rText :: Text, rInt :: Int, rDouble :: Double }
  deriving (Eq, Show)

deriveGeneric ''Summy

data SummyRep =
  LaevusRep { lInt :: Prim Auto, lText :: Prim Auto }
  |
  DexterRep { rText :: Prim Auto, rInt :: Prim Auto, rDouble :: Prim Auto }
  deriving (Eq, Show)

deriveGeneric ''SummyRep

data SumField =
  SumField {
    id :: UUID,
    f1 :: Summy
  }
  deriving (Eq, Show)

deriveGeneric ''SumField

data SumFieldRep =
  SumFieldRep {
    id :: Prim Auto,
    f1 :: Sum SummyRep
  }
  deriving (Eq, Show)

deriveGeneric ''SumFieldRep

ctorColumns :: NonEmpty Column
ctorColumns =
  genColumns' @Auto @['FieldInfo "lInt", 'FieldInfo "lText"] @[Int, Text]

ctorType :: TableStructure
ctorType =
  genCtorType @Auto @[Int, Text] @('Record "Laevus" ['FieldInfo "lInt", 'FieldInfo "lText"])

sumCtorColumns :: NonEmpty TableStructure
sumCtorColumns =
  genSumCtorsColumns @Auto @[[Int, Text], [Text, Int, Double]] @['Record "Laevus" ['FieldInfo "lInt", 'FieldInfo "lText"], 'Record "Dexter" ['FieldInfo "rText", 'FieldInfo "rInt", 'FieldInfo "rDouble"]]

sumColumns :: NonEmpty TableStructure
sumColumns =
  genSumColumns @Auto @Summy

compo :: CompositeType
compo =
  genCompositeType @Auto @Summy

struct :: TableStructure
struct =
  genTableStructure @Auto @SumField

table :: Table SumField
table =
  genTable @SumFieldRep @SumField

queryTable :: QueryTable IdQuery SumField
queryTable =
  genQueryTable @SumFieldRep @IdQuery @SumField

id' :: UUID
id' =
  Uid.uuid 555

laevus :: SumField
laevus =
  SumField id' (Laevus 2 "water")

dexter :: SumField
dexter =
  SumField id' (Dexter "water" 5 4.6)

prog ::
  Member (Store IdQuery DbError SumField) r =>
  SumField ->
  Sem r (Either (StoreError DbError) (Maybe SumField))
prog specimen = do
  runError do
    Store.upsert specimen
    Store.fetch (IdQuery id')

test_sumField :: UnitTest
test_sumField =
  integrationTest do
    test laevus
    test dexter
    where
      test specimen = do
        result <- withTestStoreGen @SumFieldRep (prog specimen)
        assertJust specimen =<< evalEither result
