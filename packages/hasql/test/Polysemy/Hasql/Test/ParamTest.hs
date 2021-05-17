module Polysemy.Hasql.Test.ParamTest where

import Hasql.Decoders (Row)
import Hasql.Encoders (Params)
import Polysemy.Db.Data.Column (Auto, Prim, Product, Sum)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree.Data.Effect (ADT, Newtype, Tycon)
import Polysemy.Db.Tree.Data.TreeMeta (TreeMeta(TreeMeta))
import Polysemy.Db.Tree.Meta
import Polysemy.Test (UnitTest, runTestAuto)

import Polysemy.Hasql.Data.QueryTable (QueryTable)
import Polysemy.Hasql.QueryParams (queryParams)
import Polysemy.Hasql.QueryRows (queryRows)
import Polysemy.Hasql.Table.QueryTable (queryTable)
import Polysemy.Hasql.Test.Error.Column.E1 ()
import Polysemy.Hasql.Tree.Table (TableTree, tableRoot)

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
  'Kind.Tree ('NamedField name) '[Prim] ('Kind.Prim Int)

type PrimDouble name =
  'Kind.Tree ('NamedField name) '[Prim] ('Kind.Prim Double)

type PrimMaybeDouble =
  'Kind.Prim (Maybe Double)

type ProddoType =
  'Kind.Prod Proddo '[PrimInt "prInt"]

type SummerConssType =
  [
    'Kind.ConUna ('NamedField "Summer1") ('Kind.Tree ('NamedField "txt") '[Prim] ('Kind.Prim Text)),
    'Kind.Con ('NamedField "Summer2") [
      PrimInt "int",
      PrimDouble "dubble"
    ]
  ]

type SummerType =
  'Kind.SumProd Summer SummerConssType

type ProddoMeta =
  'AdtProd '[ 'TreeMeta ('NamedField "prInt") Prim Int]

type SummerMeta =
  ADTMeta' (Sum SummerRep) Summer

type DatType =
  'Kind.Tree ('NamedField "Dat") '[ADT (ADTMeta' (Product DatRep) Dat) (Product DatRep)] ('Kind.Prod Dat [
    'Kind.Tree ('NamedField "double") [Tycon Maybe Double, Prim] PrimMaybeDouble,
    'Kind.Tree ('NamedField "newt") [Tycon [] Newt, Newtype Newt Text, Prim] ('Kind.Prim [Newt]),
    'Kind.Tree ('NamedField "proddo") '[ADT ProddoMeta (Product ProddoRep)] ProddoType,
    'Kind.Tree ('NamedField "summer") '[ADT SummerMeta (Sum SummerRep)] SummerType
  ])

columns_Dat_explicit ::
  TableTree DatType
columns_Dat_explicit =
  tableRoot @DatRep @Dat

queryParams_Dat :: Params Dat
queryParams_Dat =
  queryParams @DatType @Dat

queryParams_Summer :: Params Summer
queryParams_Summer =
  queryParams @('Kind.Tree ('NamedField "Summer") '[] SummerType) @Summer

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
