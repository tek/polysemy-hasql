module Polysemy.Hasql.Test.SumFieldTest where

import Generics.SOP (I, NP, NS)
import Generics.SOP.GGP (GCode)
import Hasql.Connection (Connection)
import Hasql.Decoders (Row)
import qualified Hasql.Encoders as Encoders
import Hasql.Encoders (Params)
import Hasql.Session (QueryError)
import Path (Abs, File, Path, absfile)
import Polysemy.Test (Hedgehog, UnitTest, assertJust, evalEither, evalLeft)
import Polysemy.Time (mkDatetime)
import Prelude hiding (Enum)

import Polysemy.Db.Data.Column (Auto, Enum, Flatten, NewtypePrim, Prim, PrimaryKey, Sum, UidRep)
import Polysemy.Db.Data.ColumnOptions (ColumnOptions(unique))
import Polysemy.Db.Data.Cond (LessOrEq(LessOrEq))
import Polysemy.Db.Data.CreationTime (CreationTime(CreationTime))
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.IdQuery (IdQuery(IdQuery), UuidQuery)
import Polysemy.Db.Data.Store (Store)
import Polysemy.Db.Data.StoreError (StoreError)
import qualified Polysemy.Db.Data.StoreQuery as StoreQuery
import Polysemy.Db.Data.StoreQuery (StoreQuery)
import Polysemy.Db.Data.TableStructure (Column, TableStructure)
import qualified Polysemy.Db.Data.Uid as Uid
import Polysemy.Db.Data.Uid (Uid, Uid(Uid))
import Polysemy.Db.Random (Random)
import qualified Polysemy.Db.Store as Store
import Polysemy.Hasql.Data.DbConnection (DbConnection)
import Polysemy.Hasql.Data.QueryTable (QueryTable(QueryTable))
import Polysemy.Hasql.Data.Table (Table(Table))
import Polysemy.Hasql.Database (interpretDatabase)
import Polysemy.Hasql.Query.One (interpretOneGenUidWith)
import Polysemy.Hasql.Store (interpretStoreDbFullGenUid)
import Polysemy.Hasql.Table.ColumnOptions (ExplicitColumnOptions(..))
import Polysemy.Hasql.Table.ColumnType (Single, UnconsRep)
import Polysemy.Hasql.Table.Columns (columns)
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
  ReifyRepTable,
  ReifySumType,
  Rep,
  SumColumn,
  )
import Polysemy.Hasql.Table.Table (genTable)
import Polysemy.Hasql.Table.TableStructure (genTableStructure, tableStructure)
import Polysemy.Hasql.Table.ValueEncoder (repEncoder)
import Polysemy.Hasql.Test.Database (withTestStoreGen, withTestStoreTableUidGen)
import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Resource (Resource)

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
    sNewt :: NewtypePrim (Prim Auto)
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
  DexterRep { rPath :: Prim Auto, rNewt :: NewtypePrim (Prim Auto), rNume :: Enum Auto }
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
    f1 :: Sum Auto SummyRep
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
  genRows @(UnconsRep [Prim Auto, Sum Auto (SumColumn (NestedSum (ExplicitSum Summy SummyRep)))]) @'[UUID, Summy]

queryRows_SumField :: Row SumField
queryRows_SumField =
  queryRows @SumFieldRep

repEncoder_Newt :: Encoders.Value Newt
repEncoder_Newt =
  repEncoder @(NewtypePrim (Prim Auto))

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
  queryParam @(NewtypePrim (Prim Auto))

queryParams_Sinister :: Params Sinister
queryParams_Sinister =
  queryParams @(ProdColumn (ProdCode (GCode SinisterRep)))

type SumColumns_Summy =
  [
    ProdColumn '[Prim Auto, SinisterRepFlatten],
    ProdColumn '[Prim Auto, NewtypePrim (Prim Auto), Enum Auto]
  ]

type SumColumn_Summy =
  SumColumn SumColumns_Summy

testSumField ::
  Rep SumField ~ ExplicitTable SumFieldRep SumField =>
  Rep SumField ~ ProdTable [Prim Auto, Sum Auto SumColumn_Summy] =>
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
  queryParams @(ProdTable [Prim Auto, Sum Auto SumColumn_Summy])

struct :: TableStructure
struct =
  tableStructure @SumField

structManual :: TableStructure
structManual =
  genTableStructure @SumFieldRep @SumField

table :: Table SumField
table =
  genTable @SumFieldRep

queryParams_IdQuery :: Params (IdQuery UUID)
queryParams_IdQuery =
  queryParams @(Rep (IdQuery UUID))

queryTable_SumField :: QueryTable (IdQuery UUID) SumField
queryTable_SumField =
  genQueryTable @SumFieldRep @(IdQuery UUID) @SumField

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
  Member (Store UuidQuery DbError a) r =>
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
  GenQueryTable rep UuidQuery d =>
  d ->
  Sem r ()
sumTest specimen = do
  result <- withTestStoreGen @rep @UuidQuery @d $ runError do
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
    two :: Sum Auto TwoRep,
    other :: Prim Auto
  }
  deriving (Generic)

test_simpleSumField :: UnitTest
test_simpleSumField =
  integrationTest do
    sumTest @(UidRep (Prim Auto) SimpleRep) (Uid id' (Simple (TwoA 5) 9))

data SumPK =
  SumPKL { l :: Int }
  |
  SumPKR { r :: Int }
  deriving (Eq, Show, Generic)

data SumPKRep =
  SumPKLRep { l :: Prim Auto }
  |
  SumPKRRep { r :: Prim Auto }
  deriving (Eq, Show, Generic)

data SumId =
  SumId { number :: CreationTime }
  deriving (Eq, Show, Generic)

data SumIdRep =
  SumIdRep { number :: NewtypePrim (Prim Auto) }
  deriving (Eq, Show, Generic)

data SumPKQ =
  SumPKQ {
    number :: Maybe (LessOrEq CreationTime)
  }
  deriving (Eq, Show, Generic)

type SumIdRecRep =
  UidRep (Sum Auto SumPKRep) SumIdRep

type SumIdRec =
  Uid SumPK SumId

queryParams_SumPKQuery :: Params (IdQuery SumPK)
queryParams_SumPKQuery =
  queryParams @(Rep (IdQuery SumPK)) @(IdQuery SumPK)

queryRows_SumPKQuery :: Row (IdQuery SumPK)
queryRows_SumPKQuery =
  queryRows @(Rep (IdQuery SumPK)) @(IdQuery SumPK)

queryParams_SumId :: Params SumIdRec
queryParams_SumId =
  queryParams @(Rep SumIdRec) @SumIdRec

queryRows_SumId :: Row SumIdRec
queryRows_SumId =
  queryRows @(Rep SumIdRec) @SumIdRec

columns_SumId :: [Column]
columns_SumId =
  columns @(UidRep (Sum Auto SumPKRep) SumIdRep) @SumIdRec

tableStructure_SumId :: TableStructure
tableStructure_SumId =
  genTableStructure @(UidRep (Sum Auto SumPKRep) SumIdRep) @SumIdRec

table_SumId :: Table SumIdRec
table_SumId =
  genTable @(UidRep (Sum Auto SumPKRep) SumIdRep)

queryTable_SumId ::
  ReifyRepTable (Rep SumIdRec) SumIdRec ~ ReifyRepTable SumIdRecRep SumIdRec =>
  QueryTable (IdQuery SumPK) (Uid SumPK SumId)
queryTable_SumId =
  genQueryTable @(UidRep (Sum Auto SumPKRep) SumIdRep)

queryParams_SumPKQ ::
  Rep SumPKQ ~ ProdTable '[NewtypePrim (NewtypePrim (Prim Auto))] =>
  Params SumPKQ
queryParams_SumPKQ =
  queryParams @(Rep SumPKQ)

queryTable_SumId_SumPKQ ::
  QueryTable SumPKQ (Uid SumPK SumId)
queryTable_SumId_SumPKQ =
  genQueryTable @(UidRep (Sum Auto SumPKRep) SumIdRep)

sumIdQProg ::
  Member (StoreQuery SumPKQ DbError (Maybe (Uid SumPK SumId))) r =>
  Members [Store SumPK DbError SumIdRec, DbConnection Connection, Error (StoreError DbError), Hedgehog IO, Embed IO] r =>
  Sem r ()
sumIdQProg = do
  Store.upsert specimen
  r1 <- Store.fetch (SumPKR 5)
  r2 <- StoreQuery.basicQuery (SumPKQ Nothing)
  assertJust specimen r1
  assertJust specimen r2
  where
    specimen =
      Uid (SumPKR 5) (SumId (CreationTime (mkDatetime 2020 1 1 0 0 0)))

sumIdProg ::
  Members [Store SumPK DbError SumIdRec, DbConnection Connection, Error (StoreError DbError), Hedgehog IO, Embed IO] r =>
  QueryTable (IdQuery SumPK) SumIdRec ->
  Sem r ()
sumIdProg (QueryTable (Table stct _ _) _ _) =
  interpretDatabase stct $
    interpretOneGenUidWith @SumIdRep @(Sum Auto SumPKRep) stct $
    sumIdQProg

test_sumId :: UnitTest
test_sumId =
  integrationTest do
    _ <- pure queryTable_SumId
    withTestStoreTableUidGen @SumIdRep @(Sum Auto SumPKRep) sumIdProg

data Dat1 =
  Dat1 {
    dat1_Int :: Int
  }
  deriving (Eq, Show, Generic)

data Dat1Rep =
  Dat1Rep {
    dat1_Int :: Prim Auto
  }
  deriving (Eq, Show, Generic)

data Dat2 =
  Dat2 {
    dat2_Text :: Text
  }
  deriving (Eq, Show, Generic)

data Dat2Rep =
  Dat2Rep {
    dat2_Int :: Prim Auto
  }
  deriving (Eq, Show, Generic)

test_multiSum :: UnitTest
test_multiSum =
  integrationTest do
    interpretStoreDbFullGenUid @Dat1Rep @(Sum PrimaryKey SumPKRep) @_ @Dat1 do
      Store.insert record
      interpretStoreDbFullGenUid @Dat2Rep @(Sum PrimaryKey SumPKRep) @_ @Dat2 do
        Store.insert (Uid (SumPKL 6) (Dat2 "six"))
        _ <- evalLeft =<< runError (Store.insert (Uid (SumPKL 6) (Dat2 "six")))
        result <- Store.fetchAll @SumPK @_ @(Uid SumPK Dat1)
        assertJust (pure record) result
  where
    record =
      Uid (SumPKL 5) (Dat1 5)
