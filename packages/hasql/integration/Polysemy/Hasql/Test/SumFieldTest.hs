{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.SumFieldTest where

import Hasql.Decoders (Row)
import Path (Abs, File, Path, absfile)
import Polysemy.Db.Data.ColumnOptions (ColumnOptions (unique))
import Polysemy.Db.Data.Cond (LessOrEq (LessOrEq))
import Polysemy.Db.Data.CreationTime (CreationTime (CreationTime))
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Db.Data.Rep (Auto, Flatten, IdQuery, Prim, PrimaryKey, Rep, Sum, UidNestRep, UidRep, UuidRep)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store, UuidStore)
import qualified Polysemy.Db.Data.StoreQuery as StoreQuery
import Polysemy.Db.Data.StoreQuery (StoreQuery)
import qualified Polysemy.Db.Data.Uid as Uid
import Polysemy.Db.Data.Uid (Uid (Uid), Uuid)
import Polysemy.Log (Log)
import Polysemy.Test (Hedgehog, UnitTest, assertJust, evalLeft)
import Polysemy.Time (GhcTime, mkDatetime)
import Prelude hiding (Enum)

import Polysemy.Hasql (HasqlConnection)
import Polysemy.Hasql.Data.QueryTable (QueryTable (QueryTable))
import Polysemy.Hasql.Database (interpretDatabase)
import Polysemy.Hasql.ManagedTable (interpretManagedTable)
import Polysemy.Hasql.Query (interpretQuery)
import Polysemy.Hasql.Query.One (interpretOne)
import Polysemy.Hasql.QueryRows (QueryRows, queryRows)
import Polysemy.Hasql.Store (interpretStoreDbFullGenAs)
import Polysemy.Hasql.Table.ColumnOptions (ExplicitColumnOptions (..))
import Polysemy.Hasql.Table.Query.Update (BuildPartialSql)
import Polysemy.Hasql.Table.Schema (Schema)
import Polysemy.Hasql.Test.Database (TestStoreDeps, withTestStore, withTestStoreTableGenAs)
import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Hasql.Tree.Table (TableRoot)

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
    sId :: Auto,
    sNewt :: Auto
  }
  deriving (Eq, Show, Generic)

data Summy =
  Laevus { lInt :: Int, lSinister :: Sinister }
  |
  Dexter { rPath :: Path Abs File, rNewt :: Newt, rNume :: Nume }
  deriving (Eq, Show, Generic)

data SummyRep =
  LaevusRep { lInt :: Auto, lSinister :: Flatten SinisterRep }
  |
  DexterRep { rPath :: Auto, rNewt :: Auto, rNume :: Auto }
  deriving (Eq, Show, Generic)

instance ExplicitColumnOptions SummyRep where
  explicitColumnOptions =
    def { unique = False }

type SumField =
  Uuid Summy

type SumFieldRep =
  UidNestRep Prim (Sum SummyRep)

row_queryRows_Sinister ::
  ∀ c .
  TableRoot SinisterRep Sinister c =>
  QueryRows c Sinister =>
  Row Sinister
row_queryRows_Sinister =
  queryRows @c

queryRows_SumField ::
  ∀ c .
  TableRoot SumFieldRep SumField c =>
  QueryRows c SumField =>
  Row SumField
queryRows_SumField =
  queryRows @c

id' :: UUID
id' =
  Uid.uuid 555

laevus :: SumField
laevus =
  Uid id' (Laevus 2 (Sinister (Uid.uuid 12) (Just 99)))

dexter :: SumField
dexter =
  Uid id' (Dexter [absfile|/foo/bar|] 5 Three)

prog ::
  Member (UuidStore a) r =>
  Uuid a ->
  Sem r (Either DbError (Maybe a))
prog specimen =
  runError do
    Store.upsert specimen
    fmap Uid._payload <$> Store.fetch id'

sumTest ::
  ∀ rep d tree r .
  Eq d =>
  Show d =>
  BuildPartialSql d tree =>
  Members (Hedgehog IO : TestStoreDeps) r =>
  Schema IdQuery rep UUID (Uuid d) =>
  Uuid d ->
  Sem r ()
sumTest specimen = do
  result <- withTestStore @IdQuery @rep $ restop do
    Store.upsert specimen
    Store.fetch id'
  assertJust specimen result

test_reps :: IO ()
test_reps = do
  _ <- pure row_queryRows_Sinister
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
  TwoARep { twoA :: Prim }
  |
  TwoBRep { twoB :: Prim }
  deriving (Generic)

data SimpleRep =
  SimpleRep {
    two :: Sum TwoRep,
    other :: Auto
  }
  deriving (Generic)

test_simpleSumField :: UnitTest
test_simpleSumField =
  integrationTest do
    sumTest @(UidRep (Prim) SimpleRep) (Uid id' (Simple (TwoA 5) 9))

data SumPK =
  SumPKL { l :: Int }
  |
  SumPKR { r :: Int }
  deriving (Eq, Show, Generic)

data SumPKRep =
  SumPKLRep { l :: Prim }
  |
  SumPKRRep { r :: Prim }
  deriving (Eq, Show, Generic)

data SumId =
  SumId { number :: CreationTime }
  deriving (Eq, Show, Generic)

data SumIdRep =
  SumIdRep { number :: Prim }
  deriving (Eq, Show, Generic)

data SumPKQ =
  SumPKQ {
    number :: Maybe (LessOrEq CreationTime)
  }
  deriving (Eq, Show, Generic)

type SumIdRecRep =
  UidRep (Sum SumPKRep) SumIdRep

type SumIdRec =
  Uid SumPK SumId

sumIdQProg ::
  Member (StoreQuery SumPKQ (Maybe (Uid SumPK SumId)) !! DbError) r =>
  Members [Store SumPK SumId !! DbError, HasqlConnection, Stop DbError, Hedgehog IO, Embed IO] r =>
  Sem r ()
sumIdQProg = do
  restop (Store.upsert specimen)
  r1 <- restop (Store.fetch (SumPKR 5))
  r2 <- restop (StoreQuery.basic (SumPKQ Nothing))
  assertJust specimen r1
  assertJust specimen r2
  where
    specimen =
      Uid (SumPKR 5) (SumId (CreationTime (mkDatetime 2020 1 1 0 0 0)))

sumIdProg ::
  Members [Store SumPK SumId !! DbError, HasqlConnection, Stop DbError, GhcTime, Hedgehog IO, Log, Embed IO] r =>
  Member (Error InitDbError) r =>
  QueryTable SumPK SumIdRec ->
  Sem r ()
sumIdProg (QueryTable table _ _) =
  interpretDatabase $
    interpretQuery @Auto @(UidRep (Sum SumPKRep) SumIdRep) $
    interpretManagedTable table $
    interpretOne $
    sumIdQProg

test_sumId :: UnitTest
test_sumId =
  integrationTest do
    withTestStoreTableGenAs @Auto @SumIdRep @(Sum SumPKRep) sumIdProg

data Dat1 =
  Dat1 {
    dat1_Int :: Int
  }
  deriving (Eq, Show, Generic)

data Dat1Rep =
  Dat1Rep {
    dat1_Int :: Prim
  }
  deriving (Eq, Show, Generic)

data Dat2 =
  Dat2 {
    dat2_Text :: Text
  }
  deriving (Eq, Show, Generic)

data Dat2Rep =
  Dat2Rep {
    dat2_Text :: Prim
  }
  deriving (Eq, Show, Generic)

test_multiSum :: UnitTest
test_multiSum =
  integrationTest do
    interpretStoreDbFullGenAs @Auto @(Rep '[Sum SumPKRep, PrimaryKey]) @Dat1Rep do
      restop @DbError @(Store SumPK Dat1) (Store.insert record)
      interpretStoreDbFullGenAs @Auto @(Rep '[Sum SumPKRep, PrimaryKey]) @Dat2Rep @_ @Dat2 do
        restop @DbError (Store.insert (Uid (SumPKL 6) (Dat2 "six")))
        _ <- evalLeft =<< resumeEither (Store.insert (Uid (SumPKL 6) (Dat2 "six")))
        result <- restop @DbError @(Store SumPK Dat1) Store.fetchAll
        assertJust (pure record) result
  where
    record =
      Uid (SumPKL 5) (Dat1 5)

data NumericFields =
  NumericOne Int Text
  |
  NumericTwo Double (Maybe Int)
  deriving (Eq, Show, Generic)

data NumericFieldsRep =
  NumericOneRep Prim Prim
  |
  NumericTwoRep Prim Prim
  deriving (Eq, Show, Generic)

data NumericDat =
  NumericDat { numeric :: NumericFields }
  deriving (Eq, Show, Generic)

data NumericDatRep =
  NumericDatRep {
    numeric :: Sum NumericFieldsRep
  }
  deriving (Eq, Show, Generic)

test_numericFieldSum :: UnitTest
test_numericFieldSum =
  integrationTest do
    sumTest @(UuidRep NumericDatRep) @NumericDat (Uid id' (NumericDat (NumericTwo 5.5 (Just 2))))
