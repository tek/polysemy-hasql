module Polysemy.Hasql.Test.ParamTest where

import Generics.SOP (NP(..))
import Hasql.Decoders (Row)
import Hasql.Encoders (Params)
import Polysemy.Db.Data.Column (Auto, Con, Prim, Product, Sum)
import Polysemy.Hasql.Test.Error.Column.E1 ()
import Polysemy.Test (UnitTest, runTestAuto, (===))

import Polysemy.Hasql.Data.QueryTable (QueryTable)
import Polysemy.Hasql.Table.QueryTable (queryTable)
import Polysemy.Hasql.Column.Class (tableColumn)
import Polysemy.Hasql.Column.Data.Effect (ADT, Newtype, Tc)
import Polysemy.Hasql.Column.Effect
import Polysemy.Hasql.Column.Meta
import qualified Polysemy.Hasql.Kind.Data.DbType as Kind
import Polysemy.Hasql.QueryParams (queryParams)
import Polysemy.Hasql.QueryRows (queryRows)
import qualified Polysemy.Hasql.Type.Data.DbType as Type
import Polysemy.Db.Data.FieldId (FieldId(NamedField))

data Summer =
  Summer1 { txt :: Text }
  |
  Summer2 { int :: Int, dubble :: Double }
  deriving (Eq, Show, Generic)

data SummerRep =
  SummerRep1 { txt :: Prim }
  |
  SummerRep2 { int :: Prim, dubble :: Prim }
  deriving (Eq, Show, Generic)

data Proddo =
  Proddo {
    prInt :: Int
  }
  deriving (Eq, Show, Generic)

data ProddoRep =
  ProddoRep {
    prInt :: Prim
  }
  deriving (Eq, Show, Generic)

newtype Newt =
  Newt { unNewt :: Text }
  deriving (Eq, Show, Generic)
  deriving newtype (IsString)

data Dat =
  Dat {
    double :: Maybe Double
    ,
    newt :: [Newt]
    ,
    proddo :: Proddo
    ,
    summer :: Summer
  }
  deriving (Eq, Show, Generic)

data DatRep =
  DatRep {
    double :: Prim
    ,
    newt :: Auto
    ,
    proddo :: Product ProddoRep
    ,
    summer :: Sum SummerRep
  }
  deriving (Eq, Show, Generic)

data Simple =
  Simple {
    double :: Double
  }
  deriving (Eq, Show, Generic)

type PrimInt name =
  'Kind.Column ('NamedField name) '[Prim] ('Kind.Prim Int)

type PrimDouble name =
  'Kind.Column ('NamedField name) '[Prim] ('Kind.Prim Double)

type PrimMaybeDouble =
  'Kind.Prim (Maybe Double)

type ProddoType =
  'Kind.Prod Proddo '[PrimInt "prInt"]

type SummerConssType =
  [
    PrimInt "sum_index",
    'Kind.Column ('NamedField "Summer1") '[Prim] ('Kind.Prim Text),
    'Kind.Column ('NamedField "Summer2") '[] ('Kind.Prod (Con ('NamedField "Summer2")) [
      PrimInt "int",
      PrimDouble "dubble"
    ])
  ]

type SummerType =
  'Kind.Sum Summer SummerConssType

type ProddoMeta =
  'ADTProd '[ 'ColumnMeta ('NamedField "prInt") Prim Int]

type SummerMeta =
  ADTMeta' (Sum SummerRep) Summer

type DatType =
  'Kind.Column ('NamedField "Dat") '[ADT (ADTMeta' (Product DatRep) Dat) (Product DatRep)] ('Kind.Prod Dat [
    'Kind.Column ('NamedField "double") [Tc Maybe Double, Prim] PrimMaybeDouble,
    'Kind.Column ('NamedField "newt") [Tc [] Newt, Newtype Newt Text, Prim] ('Kind.Prim [Newt]),
    'Kind.Column ('NamedField "proddo") '[ADT ProddoMeta (Product ProddoRep)] ProddoType,
    'Kind.Column ('NamedField "summer") '[ADT SummerMeta (Sum SummerRep)] SummerType
  ])

columns_Dat_explicit ::
  Type.Column DatType
columns_Dat_explicit =
  tableColumn @DatRep @Dat

queryParams_Dat :: Params Dat
queryParams_Dat =
  queryParams @DatType @Dat

queryParams_Summer :: Params Summer
queryParams_Summer =
  queryParams @('Kind.Column ('NamedField "Summer") '[] SummerType) @Summer

queryRows_Dat :: Row Dat
queryRows_Dat =
  queryRows @DatType @Dat

data DoubleQ =
  DoubleQ {
    double :: Double
  }
  deriving (Eq, Show, Generic)

queryTable_Simple_Auto :: QueryTable DoubleQ Simple
queryTable_Simple_Auto =
  queryTable @DoubleQ @Simple

queryTable_Dat_Auto :: QueryTable DoubleQ Dat
queryTable_Dat_Auto =
  queryTable @DoubleQ @Dat

test_param2 :: UnitTest
test_param2 =
  runTestAuto do
    unit
