module Polysemy.Hasql.Test.SumFieldTest where

import Generics.SOP (I, NP, NS)
import Generics.SOP.GGP (GCode)
import Hasql.Connection (Connection)
import Hasql.Decoders (Row)
import qualified Hasql.Encoders as Encoders
import Hasql.Encoders (Params)
import Hasql.Session (QueryError)
import Path (Abs, File, Path, absfile)
import Prelude hiding (Enum)

import Polysemy.Db.Data.Column (Auto, Enum, Flatten, NewtypePrim, PK', PK(PK), PKRep, PKRep', Prim, PrimaryKey, Sum)
import Polysemy.Db.Data.ColumnOptions (ColumnOptions(unique))
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.Store (Store)
import Polysemy.Db.Data.StoreError (StoreError)
import Polysemy.Db.Data.TableStructure (TableStructure)
import qualified Polysemy.Db.Data.Uid as Uid
import Polysemy.Db.Random (Random)
import qualified Polysemy.Db.Store as Store
import Polysemy.Hasql.Data.DbConnection (DbConnection)
import Polysemy.Hasql.Data.QueryTable (QueryTable)
import Polysemy.Hasql.Data.Schema (IdQuery(IdQuery))
import Polysemy.Hasql.Data.Table (Table)
import Polysemy.Hasql.Table.ColumnOptions (ExplicitColumnOptions(..))
import Polysemy.Hasql.Table.ColumnType (Single, UnconsRep)
import Polysemy.Hasql.Table.QueryParam (queryParam, writeNulls, writeNulls2)
import Polysemy.Hasql.Table.QueryParams (columnParams, productParams, queryParams, sumParams)
import Polysemy.Hasql.Table.QueryRow (readNulls)
import Polysemy.Hasql.Table.QueryRows (genQueryRows, genRows, queryRows, sumRows)
import Polysemy.Hasql.Table.QueryTable (GenQueryTable, genQueryTable)
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
import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Resource (Resource)
import Polysemy.Test (Hedgehog, UnitTest, evalEither)
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
  Dexter { rPath :: Path Abs File, rNewt :: Newt, rNume :: Nume }
  deriving (Eq, Show, Generic)

data SummyRep =
  LaevusRep { lInt :: Prim Auto, lSinister :: Flatten SinisterRep }
  |
  DexterRep { rPath :: Prim Auto, rNewt :: NewtypePrim Auto, rNume :: Enum Auto }
  deriving (Eq, Show, Generic)

instance ExplicitColumnOptions SummyRep where
  explicitColumnOptions =
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

readNulls_Flatten_Sinister :: Row ()
readNulls_Flatten_Sinister =
  readNulls @(ProdColumn '[SinisterRepFlatten]) @'[Sinister]

readNulls_Summy :: Row ()
readNulls_Summy =
  readNulls @(ProdColumn [Prim Auto, SinisterRepFlatten]) @'[Int, Sinister]

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

queryParams_IdQuery :: Params IdQuery
queryParams_IdQuery =
  queryParams @(Rep IdQuery)

queryTable_SumField :: QueryTable IdQuery SumField
queryTable_SumField =
  genQueryTable @SumFieldRep @IdQuery @SumField

id' :: UUID
id' =
  Uid.uuid 555

laevus :: SumField
laevus =
  SumField id' (Laevus 2 (Sinister (Uid.uuid 12) (Just 99)))

dexter :: SumField
dexter =
  SumField id' (Dexter [absfile|/foo/bar|] 5 Three)

prog ::
  Member (Store IdQuery DbError a) r =>
  a ->
  Sem r (Either (StoreError DbError) (Maybe a))
prog specimen =
  runError do
    Store.upsert specimen
    Store.fetch (IdQuery id')

sumTest ::
  ∀ rep d r .
  Show d =>
  Eq d =>
  Members [Hedgehog IO, Resource, Embed IO, (DbConnection Connection), Random, Error QueryError, Error DbError] r =>
  GenQueryTable rep IdQuery d =>
  d ->
  Sem r ()
sumTest specimen = do
  result <- withTestStoreGen @rep @IdQuery @d $ runError do
    Store.upsert specimen
    Store.fetch (IdQuery id')
  assertJust specimen =<< evalEither result

test_reps :: IO ()
test_reps = do
  _ <- pure testSumField
  unit

test_sumField :: UnitTest
test_sumField =
  integrationTest do
    sumTest @SumFieldRep laevus
    sumTest @SumFieldRep dexter

data Two =
  TwoA { twoA :: Int }
  |
  TwoB { twoB :: Int }
  deriving (Eq, Show, Generic)

data Simple =
  Simple {
    two :: Two,
    other :: Int
  }
  deriving (Eq, Show, Generic)

data TwoRep =
  TwoARep { twoA :: Prim Auto }
  |
  TwoBRep { twoB :: Prim Auto }
  deriving (Generic)

data SimpleRep =
  SimpleRep {
    two :: Sum TwoRep,
    other :: Prim Auto
  }
  deriving (Generic)

test_simpleSumField :: UnitTest
test_simpleSumField =
  integrationTest do
    sumTest @(PKRep Prim UUID SimpleRep) (PK @Prim id' (Simple (TwoA 5) 9))

data Uid i a =
  Uid {
    id :: i,
    payload :: a
  }
  deriving (Eq, Show, Generic, Functor)

data SumPK =
  SumPKL { l :: Int }
  |
  SumPKR { r :: Int }
  deriving (Eq, Show, Generic)

data SumPKQuery =
  SumPKQuery {
    id :: SumPK
  }
  deriving (Eq, Show, Generic)

data SumPKRep r =
  SumPKLRep { l :: Prim r }
  |
  SumPKRRep { r :: Prim r }
  deriving (Eq, Show, Generic)

data SumId =
  SumId { number :: Int }
  deriving (Eq, Show, Generic)

type SumIdRecRep =
  PKRep' (Sum (SumPKRep PrimaryKey)) SumPK SumId

type SumIdRec =
  PK' SumPK SumId

queryParams_SumPKQuery :: Params SumPKQuery
queryParams_SumPKQuery =
  queryParams @(Rep SumPKQuery) @SumPKQuery

queryRows_SumPKQuery :: Row SumPKQuery
queryRows_SumPKQuery =
  queryRows @(Rep SumPKQuery) @SumPKQuery

queryParams_SumId :: Params SumIdRec
queryParams_SumId =
  queryParams @(Rep SumIdRec) @SumIdRec

queryRows_SumId :: Row SumIdRec
queryRows_SumId =
  queryRows @(Rep SumIdRec) @SumIdRec

tableStructure_SumId :: TableStructure
tableStructure_SumId =
  genTableStructure @(Rep SumIdRec) @SumIdRec

table_SumId :: Table SumIdRec
table_SumId =
  genTable @(Rep SumIdRec) @SumIdRec

queryTable_SumId :: QueryTable SumPKQuery (PK' SumPK SumId)
queryTable_SumId =
  genQueryTable @(Rep SumIdRec)

test_sumId :: UnitTest
test_sumId =
  integrationTest do
    result <- withTestStoreGen @(Rep SumIdRec) @SumPKQuery $ runError do
      Store.upsert specimen
      Store.fetch (SumPKQuery (SumPKR 5))
    assertJust specimen =<< evalEither result
    where
      specimen =
        Uid (SumPKR 5) (SumId 10)
