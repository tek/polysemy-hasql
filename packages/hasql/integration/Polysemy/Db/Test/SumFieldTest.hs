module Polysemy.Db.Test.SumFieldTest where

import Generics.SOP (I, NP, NS)
import Generics.SOP.GGP (GCode)
import Hasql.Decoders (Row)
import qualified Hasql.Encoders as Encoders
import Hasql.Encoders (Params)
import Prelude hiding (Enum)

import Polysemy.Db.Data.Column (Auto, Enum, Flatten, NewtypePrim, Prim, Sum)
import Polysemy.Db.Data.ColumnParams (ColumnParams(unique))
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.Store (Store)
import Polysemy.Db.Data.StoreError (StoreError)
import Polysemy.Db.Data.TableStructure (TableStructure)
import qualified Polysemy.Db.Data.Uid as Uid
import qualified Polysemy.Db.Store as Store
import Polysemy.Db.Test.Run (integrationTest)
import Polysemy.Hasql.Data.QueryTable (QueryTable)
import Polysemy.Hasql.Data.Schema (IdQuery(IdQuery))
import Polysemy.Hasql.Data.Table (Table)
import Polysemy.Hasql.Table.ColumnParams (ExplicitColumnParams(..))
import Polysemy.Hasql.Table.ColumnType (Single, UnconsRep)
import Polysemy.Hasql.Table.QueryParam (queryParam, writeNulls, writeNulls2)
import Polysemy.Hasql.Table.QueryParams (columnParams, productParams, queryParams, sumParams)
import Polysemy.Hasql.Table.QueryRow (readNulls2)
import Polysemy.Hasql.Table.QueryRows (genQueryRows, genRows, queryRows, sumRows)
import Polysemy.Hasql.Table.QueryTable (genQueryTable)
import Polysemy.Hasql.Table.Representation (
  ExplicitSum,
  ExplicitTable,
  NestedSum,
  ProdCode,
  ProdColumn,
  ProdTable,
  ReifySumType,
  Rep,
  SumColumn,
  )
import Polysemy.Hasql.Table.Table (genTable)
import Polysemy.Hasql.Table.TableStructure (genTableStructure, tableStructure)
import Polysemy.Hasql.Table.ValueEncoder (repEncoder)
import Polysemy.Hasql.Test.Database (withTestStoreGen)
import Polysemy.Test (UnitTest, evalEither)
import Polysemy.Test.Hedgehog (assertJust)

data Nume =
  One
  |
  Two
  |
  Three
  deriving (Eq, Show, Generic)

newtype Newt =
  Newt Int
  deriving (Eq, Show, Generic)
  deriving newtype (Num)

data Sinister =
  Sinister {
     sId :: UUID,
     sNewt :: Maybe Newt
  }
  deriving (Eq, Show, Generic)

data SinisterRep =
  SinisterRep {
    sId :: Prim Auto,
    sNewt :: NewtypePrim Auto
  }
  deriving (Eq, Show, Generic)

data Summy =
  Laevus { lInt :: Int, lSinister :: Sinister }
  |
  Dexter { rText :: Text, rNewt :: Newt, rNume :: Nume }
  deriving (Eq, Show, Generic)

data SummyRep =
  LaevusRep { lInt :: Prim Auto, lSinister :: Flatten SinisterRep }
  |
  DexterRep { rText :: Prim Auto, rNewt :: NewtypePrim Auto, rNume :: Enum Auto }
  deriving (Eq, Show, Generic)

instance ExplicitColumnParams SummyRep where
  explicitColumnParams =
    def { unique = False }

data SumField =
  SumField {
    id :: UUID,
    f1 :: Summy
  }
  deriving (Eq, Show, Generic)

data SumFieldRep =
  SumFieldRep {
    id :: Prim Auto,
    f1 :: Sum SummyRep
  }
  deriving (Eq, Show, Generic)

row_queryRows_Sinister :: Row Sinister
row_queryRows_Sinister =
  queryRows @SinisterRep

type SinisterRepFlatten =
  Flatten (ProdColumn (ProdCode (GCode SinisterRep)))

row_genRows_Laevus :: NP Row [Int, Sinister]
row_genRows_Laevus =
  genRows @(UnconsRep [Prim Auto, SinisterRepFlatten]) @[Int, Sinister]

readNulls2_Flatten_Sinister :: Row ()
readNulls2_Flatten_Sinister =
  readNulls2 @(ProdColumn '[SinisterRepFlatten]) @'[Sinister]

readNulls2_Summy :: Row ()
readNulls2_Summy =
  readNulls2 @(ProdColumn [Prim Auto, SinisterRepFlatten]) @'[Int, Sinister]

row_sumRows_Sinister :: Row (NS (NP I) '[ '[Sinister]])
row_sumRows_Sinister =
  sumRows @'[ ProdColumn '[SinisterRepFlatten]] @'[ '[Sinister]] 0

row_sumRows_Summy :: Row (NS (NP I) '[ '[Int, Sinister], '[Text, Int, Double]])
row_sumRows_Summy =
  sumRows @'[
    ProdColumn '[Prim Auto, SinisterRepFlatten],
    ProdColumn '[Prim Auto, Prim Auto, Prim Auto]
  ] @'[ '[Int, Sinister], '[Text, Int, Double]] 0

row_genQuery_Summy :: Row Summy
row_genQuery_Summy =
  genQueryRows @(ReifySumType (SumColumn (NestedSum (ExplicitSum Summy SummyRep))) Summy) @Summy @(GCode Summy)

row_genRows_SumField :: NP Row '[UUID, Summy]
row_genRows_SumField =
  genRows @(UnconsRep [Prim Auto, Sum (SumColumn (NestedSum (ExplicitSum Summy SummyRep)))]) @'[UUID, Summy]

queryRows_SumField :: Row SumField
queryRows_SumField =
  queryRows @SumFieldRep

repEncoder_Newt :: Encoders.Value Newt
repEncoder_Newt =
  repEncoder @(NewtypePrim Auto)

writeNulls_Sinister :: Params Sinister
writeNulls_Sinister =
  writeNulls @(ProdCode (GCode SinisterRep)) @(ProdCode (GCode Sinister))

writeNulls2_SumField :: Params SumField
writeNulls2_SumField =
  writeNulls2 @[
    ProdColumn '[Prim Auto, SinisterRepFlatten],
    ProdColumn '[Prim Auto, Prim Auto, Prim Auto]
  ] @[ [Int, Sinister], [Text, Int, Double]]

queryParam_Newt :: Params (Maybe Newt)
queryParam_Newt =
  queryParam @(NewtypePrim Auto)

queryParams_Sinister :: Params Sinister
queryParams_Sinister =
  queryParams @(ProdColumn (ProdCode (GCode SinisterRep)))

type SumColumns_Summy =
  [
    ProdColumn '[Prim Auto, SinisterRepFlatten],
    ProdColumn '[Prim Auto, NewtypePrim Auto, Enum Auto]
  ]

type SumColumn_Summy =
  SumColumn SumColumns_Summy

testSumField ::
  Rep SumField ~ ExplicitTable SumFieldRep SumField =>
  Rep SumField ~ ProdTable [Prim Auto, Sum SumColumn_Summy] =>
  ()
testSumField =
  ()

productParams_Newt :: NP Params '[Newt, Double]
productParams_Newt =
  productParams @(Single (NewtypePrim Auto) '[Prim Auto])

writeNulls_Newt :: Params ()
writeNulls_Newt =
  writeNulls @'[NewtypePrim Auto] @'[Newt]

writeNulls_Summy2 :: Params ()
writeNulls_Summy2 =
  writeNulls @[NewtypePrim Auto, Prim Auto] @[Newt, Double]

sumParams_Summy1 :: Params (NS (NP I) (GCode Summy))
sumParams_Summy1 =
  sumParams @SumColumns_Summy

columnParams_Summy :: Params Summy
columnParams_Summy =
  columnParams @SumColumn_Summy

queryParams_SumField :: Params SumField
queryParams_SumField =
  queryParams @(ProdTable [Prim Auto, Sum SumColumn_Summy])

struct :: TableStructure
struct =
  tableStructure @SumField

structManual :: TableStructure
structManual =
  genTableStructure @SumFieldRep @SumField

table :: Table SumField
table =
  genTable @SumFieldRep

queryTable :: QueryTable IdQuery SumField
queryTable =
  genQueryTable @SumFieldRep @IdQuery @SumField

id' :: UUID
id' =
  Uid.uuid 555

laevus :: SumField
laevus =
  SumField id' (Laevus 2 (Sinister (Uid.uuid 12) (Just 99)))

dexter :: SumField
dexter =
  SumField id' (Dexter "water" 5 Three)

prog ::
  Member (Store IdQuery DbError SumField) r =>
  SumField ->
  Sem r (Either (StoreError DbError) (Maybe SumField))
prog specimen = do
  runError do
    Store.upsert specimen
    Store.fetch (IdQuery id')

test_reps :: IO ()
test_reps = do
  _ <- pure testSumField
  unit

test_sumField :: UnitTest
test_sumField =
  integrationTest do
    test laevus
    test dexter
    where
      test specimen = do
        result <- withTestStoreGen @SumFieldRep (prog specimen)
        assertJust specimen =<< evalEither result
