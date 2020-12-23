{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.RepTest where

import Polysemy.Db.Data.Column (Auto, Con, Enum, Flatten, ForcePrim, Prim, PrimaryKey, Product, Rep, Sum)
import Polysemy.Db.Data.ColumnOptions (notNull)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import Polysemy.Hasql.Test.Error.Column.E1 ()
import Polysemy.Test (UnitTest, runTestAuto, (===))
import Prelude hiding (Enum)

import Polysemy.Hasql.Column.Class (column, tableColumn)
import Polysemy.Hasql.ColumnType (ColumnType(..))
import Polysemy.Hasql.Column.Data.Effect (ADT, Newtype, Tc)
import Polysemy.Hasql.Column.DataColumn (dataTable)
import Polysemy.Hasql.Column.Effect (
  D(D),
  Effs(Effs),
  IsADT,
  MaybeADTResolves,
  NewtypeOrADT,
  ResolveColumnEffects,
  ResolveRep,
  T(T),
  )
import Polysemy.Hasql.Column.Meta (
  ADTMeta,
  ADTMeta',
  ADTMetadata(ADTProd),
  ADTRep,
  ColumnMeta(ColumnMeta),
  MaybeADT(MaybeADT),
  )
import qualified Polysemy.Hasql.Data.DbType as Data
import qualified Polysemy.Hasql.Kind.Data.DbType as Kind
import qualified Polysemy.Hasql.Type.Data.DbType as Type

newtype Newt =
  Newt { unNewt :: Text }
  deriving (Eq, Show, Generic)

newtype NewtPrim =
  NewtPrim { unNewt :: Text }
  deriving (Eq, Show, Generic)

instance ColumnType NewtPrim where columnType = "text"

data Custom =
  Custom {
    custInt :: Int,
    custText :: Text
  }
  deriving (Eq, Show, Generic)

instance ColumnType Custom where columnType = "text"

data Summer =
  Summer1 { txt :: Text }
  |
  Summer2 { int :: Int, double :: Double }
  deriving (Eq, Show, Generic)

data SummerRep =
  SummerRep1 { txt :: Prim }
  |
  SummerRep2 { int :: Prim, double :: Prim }
  deriving (Eq, Show, Generic)

data Proddo =
  Proddo {
    prInt :: Int
  }
  deriving (Eq, Show, Generic)

data ProddoRep =
  ProddoRep {
    prInt :: Auto
  }
  deriving (Eq, Show, Generic)

data Flatty =
  Flatty {
    flat1 :: Int,
    flat2 :: Text
  }
  deriving (Eq, Show, Generic)

data FlattyRep =
  FlattyRep {
    flat1 :: Prim,
    flat2 :: Prim
  }
  deriving (Eq, Show, Generic)

data Nummo =
  Num1
  |
  Num2
  deriving (Eq, Show, Generic)

data Dat =
  Dat {
    double :: Maybe Double
    ,
    proddo :: Proddo
    ,
    summer :: Summer
    ,
    custom :: Custom
    ,
    newt :: Maybe Newt
    ,
    nummo :: [Nummo]
    ,
    flatty :: Flatty
  }
  deriving (Eq, Show, Generic)

data DatRep =
  DatRep {
    double :: Prim
    ,
    proddo :: Product ProddoRep
    ,
    summer :: Sum SummerRep
    ,
    custom :: Auto
    ,
    newt :: Auto
    ,
    nummo :: Auto
    ,
    flatty :: Flatten FlattyRep
  }
  deriving (Eq, Show, Generic)

type ProddoMeta =
  'ADTProd '[ 'ColumnMeta ('NamedField "prInt") Auto Int]

type FlattyMeta =
  'ADTProd '[ 'ColumnMeta ('NamedField "flat1") Prim Int, 'ColumnMeta ('NamedField "flat2") Prim Text]

type FlattyMetaAuto =
  'ADTProd '[ 'ColumnMeta ('NamedField "flat1") Auto Int, 'ColumnMeta ('NamedField "flat2") Auto Text]

type PrimDouble name =
  'Kind.Column ('NamedField name) '[Prim] ('Kind.Prim Double)

type PrimMaybeDouble name =
  'Kind.Column ('NamedField name) '[ Tc Maybe Double, Prim] ('Kind.Prim (Maybe Double))

type PrimInt name =
  'Kind.Column ('NamedField name) '[Prim] ('Kind.Prim Int)

type PrimText name =
  'Kind.Column ('NamedField name) '[Prim] ('Kind.Prim Text)

type ProddoType name =
  'Kind.Column ('NamedField name) '[ADT ProddoMeta (Product ProddoRep)] ('Kind.Prod Proddo '[PrimInt "prInt"])

type NewtType =
  'Kind.Column ('NamedField "newt") [Tc Maybe Newt, Newtype Newt Text, Prim] ('Kind.Prim (Maybe Newt))

type SummerConssType =
  [
    'Kind.Column ('NamedField "sum_index") '[Prim] ('Kind.Prim Int),
    'Kind.Column ('NamedField "Summer1") '[Prim] ('Kind.Prim Text),
    'Kind.Column ('NamedField "Summer2") '[] ('Kind.Prod (Con ('NamedField "Summer2")) [
    'Kind.Column ('NamedField "int") '[Prim] ('Kind.Prim Int),
    'Kind.Column ('NamedField "double") '[Prim] ('Kind.Prim Double)
    ])
  ]

type SummerMeta =
  ADTMeta' (Sum SummerRep) Summer

type DatType =
  'Kind.Column ('NamedField "Dat") '[ ADT (ADTMeta' (Rep '[Product DatRep]) Dat) (Product DatRep)] (
    'Kind.Prod Dat '[
      PrimMaybeDouble "double"
      ,
      ProddoType "proddo"
      ,
      'Kind.Column ('NamedField "summer") '[ ADT SummerMeta (Sum SummerRep)] ('Kind.Sum Summer SummerConssType)
      ,
      'Kind.Column ('NamedField "custom") '[Prim] ('Kind.Prim Custom)
      ,
      NewtType
      ,
      'Kind.Column ('NamedField "nummo") '[Tc [] Nummo, Enum] ('Kind.Prim [Nummo])
      ,
      'Kind.Column ('NamedField "flatty") '[ADT FlattyMeta (Flatten FlattyRep)] ('Kind.Prod Flatty [
        PrimInt "flat1",
        PrimText "flat2"
      ])
    ]
  )

type ProddoTypeAuto =
  'Kind.Prod Proddo '[PrimInt "prInt"]

type SummerMetaAuto =
  ADTMeta' Auto Summer

type DatTypeAuto =
  'Kind.Column ('NamedField "Dat") '[ ADT (ADTMeta' Auto Dat) Auto] (
    'Kind.Prod Dat [
      PrimMaybeDouble "double",
      'Kind.Column ('NamedField "proddo") '[ ADT ProddoMeta Auto] ProddoTypeAuto,
      'Kind.Column ('NamedField "summer") '[ ADT SummerMetaAuto Auto] ('Kind.Sum Summer SummerConssType),
      'Kind.Column ('NamedField "custom") '[Prim] ('Kind.Prim Custom),
      NewtType,
      'Kind.Column ('NamedField "nummo") '[Tc [] Nummo, Enum] ('Kind.Prim [Nummo]),
      'Kind.Column ('NamedField "flatty") '[ADT FlattyMetaAuto Auto] ('Kind.Prod Flatty [PrimInt "flat1", PrimText "flat2"])
    ]
  )

type PR =
  '[Product ProddoRep]

column_Int :: Type.Column ('Kind.Column ('NamedField "int") '[Prim] ('Kind.Prim Int))
column_Int =
  column @('ColumnMeta ('NamedField "int") (Rep '[Prim]) Int)

column_Double :: Type.Column (PrimDouble "double")
column_Double =
  column @('ColumnMeta ('NamedField "double") Auto Double)

column_Newt :: Type.Column ('Kind.Column ('NamedField "newt") '[Newtype Newt Text, Prim] ('Kind.Prim Newt))
column_Newt =
  column @('ColumnMeta ('NamedField "newt") Auto Newt)

column_Newt_Prim :: Type.Column ('Kind.Column ('NamedField "newt") '[PrimaryKey, Prim] ('Kind.Prim NewtPrim))
column_Newt_Prim =
  column @('ColumnMeta ('NamedField "newt") (Rep '[ForcePrim NewtPrim, PrimaryKey]) NewtPrim)

column_Proddo :: Type.Column (ProddoType "proddo")
column_Proddo =
  column @('ColumnMeta ('NamedField "proddo") (Rep PR) Proddo)

columns_Dat_explicit ::
  Type.Column DatType
columns_Dat_explicit =
  tableColumn @(Product DatRep) @Dat

columns_Dat_Auto ::
  Type.Column DatTypeAuto
columns_Dat_Auto =
  tableColumn @Auto @Dat

effectfulTest ::
  ADTRep (Rep PR) ~ ProddoRep =>
  ADTMeta (Rep PR) Proddo ~ 'MaybeADT ProddoMeta =>
  IsADT (Rep PR) Proddo ('Just ProddoMeta) =>
  MaybeADTResolves (ADTMeta (Rep '[]) Text) 'Nothing =>
  ResolveColumnEffects (Rep '[ForcePrim Newt]) Newt '[Prim] Newt =>
  ResolveColumnEffects Auto (Maybe [Newt]) '[Tc Maybe [Newt], Tc [] Newt, Newtype Newt Text, Prim] Text =>
  ResolveColumnEffects (Rep '[Tc Maybe [Newt], Tc [] Newt, Newtype Newt Text]) (Maybe [Newt]) '[Tc Maybe [Newt], Tc [] Newt, Newtype Newt Text, Prim] Text =>
  ResolveColumnEffects Auto Proddo '[ADT ProddoMeta Auto] Proddo =>
  ResolveColumnEffects (Rep '[Product Auto]) Proddo '[ADT ProddoMeta (Product Auto)] Proddo =>
  ResolveColumnEffects (Rep '[Product ProddoRep]) Proddo '[ADT ProddoMeta (Product ProddoRep)] Proddo =>
  ResolveColumnEffects (Rep '[Product ProddoRep]) (Maybe Proddo) '[Tc Maybe Proddo, ADT ProddoMeta (Product ProddoRep)] Proddo =>
  ResolveColumnEffects Auto (Maybe Proddo) '[Tc Maybe Proddo, ADT ProddoMeta Auto] Proddo =>
  ResolveColumnEffects Auto (Maybe [Proddo]) '[Tc Maybe [Proddo], Tc [] Proddo, ADT ProddoMeta Auto] Proddo =>
  ResolveRep (Rep '[]) ('D Custom) ('Effs '[Prim]) ('T Custom) =>
  NewtypeOrADT ('Left PR) ('D Proddo) ('Effs '[ADT ProddoMeta (Product ProddoRep)]) ('T Proddo) =>
  ()
effectfulTest =
  ()

datTargetWith :: [Data.Column] -> Data.Column
datTargetWith flattyColumns =
  Data.Column "dat" "\"dat\"" "dat" def $ Data.Prod $ [
    Data.Column "double" "\"double\"" "double precision" def { notNull = False } Data.Prim,
    Data.Column "proddo" "\"proddo\"" "proddo" def (Data.Prod [
      Data.Column "pr_int" "(\"proddo\").\"pr_int\"" "bigint" def Data.Prim
    ]),
    Data.Column "summer" "\"summer\"" "summer" def (Data.Sum [
      Data.Column "sum_index" "(\"summer\").\"sum_index\"" "bigint" def (Data.Prim),
      Data.Column "summer1" "(\"summer\").\"summer1\"" "text" def Data.Prim,
      Data.Column "summer2" "(\"summer\").\"summer2\"" "summer2" def (Data.Prod [
        Data.Column "int" "(\"summer\").\"summer2\".\"int\"" "bigint" def Data.Prim,
        Data.Column "double" "(\"summer\").\"summer2\".\"double\"" "double precision" def Data.Prim
      ])
    ]),
    Data.Column "custom" "\"custom\"" "text" def Data.Prim,
    Data.Column "newt" "\"newt\"" "text" def { notNull = False } Data.Prim,
    Data.Column "nummo" "\"nummo\"" "text[]" def Data.Prim
  ] <> flattyColumns

test_rep :: UnitTest
test_rep =
  runTestAuto do
    void (pure effectfulTest)
    datTarget === dataTable datCols
    datTargetAuto === dataTable (tableColumn @Auto @Dat)
    intTarget === intCol
    proddoTarget === proddoCol
  where
    datTarget =
      datTargetWith [
        Data.Column "flat1" "\"flat1\"" "bigint" def Data.Prim,
        Data.Column "flat2" "\"flat2\"" "text" def Data.Prim
      ]
    datTargetAuto =
      datTargetWith [
        Data.Column "flatty" "\"flatty\"" "flatty" def (Data.Prod [
          Data.Column "flat1" "(\"flatty\").\"flat1\"" "bigint" def Data.Prim,
          Data.Column "flat2" "(\"flatty\").\"flat2\"" "text" def Data.Prim
        ])
      ]
    datCols =
      tableColumn @(Product DatRep) @Dat
    intTarget =
      Data.Column "int" "\"int\"" "bigint" def Data.Prim
    intCol =
      dataTable (column @('ColumnMeta ('NamedField "int") (Prim) Int))
    proddoTarget =
      Data.Column "proddo" "\"proddo\"" "proddo" def (Data.Prod [
        Data.Column "pr_int" "\"pr_int\"" "bigint" def Data.Prim
      ])
    proddoCol =
      dataTable (column @('ColumnMeta ('NamedField "proddo") (Product ProddoRep) Proddo))
