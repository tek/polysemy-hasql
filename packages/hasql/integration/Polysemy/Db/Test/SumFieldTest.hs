module Polysemy.Db.Test.SumFieldTest where

import Generics.SOP (I, NP, NS)
import Generics.SOP.GGP (GCode)
import Hasql.Decoders (Row)

import Polysemy.Db.Data.Column (Auto, Flatten, Prim, Sum)
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
import Polysemy.Hasql.Table.QueryRows (genQueryRows, genRows, queryRows, readNulls2, sumRows)
import Polysemy.Hasql.Table.QueryTable (genQueryTable)
import Polysemy.Hasql.Table.Representation (ExplicitSum, NestedSum, ProdCode, ProdColumn, ReifySumType, SumColumn)
import Polysemy.Hasql.Table.Table (genTable)
import Polysemy.Hasql.Table.TableStructure (genTableStructure, tableStructure)
import Polysemy.Hasql.Test.Database (withTestStoreGen)
import Polysemy.Test (UnitTest, evalEither)
import Polysemy.Test.Hedgehog (assertJust)

data Sinister =
  Sinister {
     sId :: UUID,
     sText :: Maybe Text
  }
  deriving (Eq, Show, Generic)

deriveGeneric ''Sinister

data SinisterRep =
  SinisterRep {
    sId :: Prim Auto,
    sText :: Prim Auto
  }
  deriving (Eq, Show, Generic)

deriveGeneric ''SinisterRep

data Summy =
  Laevus { lInt :: Int, lSinister :: Sinister }
  |
  Dexter { rText :: Text, rInt :: Int, rDouble :: Double }
  deriving (Eq, Show, Generic)

deriveGeneric ''Summy

data SummyRep =
  LaevusRep { lInt :: Prim Auto, lSinister :: Flatten SinisterRep }
  |
  DexterRep { rText :: Prim Auto, rInt :: Prim Auto, rDouble :: Prim Auto }
  deriving (Eq, Show, Generic)

deriveGeneric ''SummyRep

instance ExplicitColumnParams SummyRep where
  explicitColumnParams =
    def { unique = False }

data SumField =
  SumField {
    id :: UUID,
    f1 :: Summy
  }
  deriving (Eq, Show, Generic)

deriveGeneric ''SumField

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
  genRows @[Prim Auto, SinisterRepFlatten] @[Int, Sinister]

nulls2_Flatten_Sinister :: Row ()
nulls2_Flatten_Sinister =
  readNulls2 @(ProdColumn '[SinisterRepFlatten]) @'[Sinister]

nulls2_Flatten_Summy :: Row ()
nulls2_Flatten_Summy =
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
  genRows @'[Prim Auto, Sum SummyRep] @'[UUID, Summy]

row_genQueryRows_SumField :: Row SumField
row_genQueryRows_SumField =
  genQueryRows @(ProdColumn (ProdCode (GCode SumFieldRep))) @SumField @(GCode SumField)

row_query :: Row SumField
row_query =
  queryRows @SumFieldRep

struct :: TableStructure
struct =
  tableStructure @SumField

structManual :: TableStructure
structManual =
  genTableStructure @SumFieldRep @SumField

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
  SumField id' (Laevus 2 (Sinister (Uid.uuid 12) (Just "water")))

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
