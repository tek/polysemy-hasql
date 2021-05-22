{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.RepTest where

import Polysemy.Db.Data.Column (Auto, Enum, Flatten, ForcePrim, Prim, PrimaryKey, Product, Rep, Sum)
import Polysemy.Db.Data.ColumnOptions (notNull)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree (tree)
import Polysemy.Db.Tree.Data.Effect (ADT, Newtype, Tycon)
import Polysemy.Db.Tree.Data.TreeMeta (TreeMeta(TreeMeta))
import Polysemy.Db.Tree.Meta (
  ADTMeta,
  ADTMeta',
  ADTRep,
  AdtMetadata(AdtProd),
  MaybeADT(MaybeADT),
  )
import Polysemy.Test (UnitTest, runTestAuto, (===))
import Prelude hiding (Enum)

import Polysemy.Hasql.Column.DataColumn (dataTable)
import Polysemy.Hasql.Column.Effect (
  D(D),
  Effs(Effs),
  IsADT,
  MaybeADTResolves,
  NewtypeOrADT,
  ResolveColumnEffects,
  ResolveRep,
  )
import Polysemy.Hasql.ColumnType (ColumnType(..))
import qualified Polysemy.Hasql.Data.DbType as Data
import Polysemy.Hasql.Test.Error.Column.E1 ()
import Polysemy.Hasql.Tree.Table (TableParams, TableTree, tableRoot)

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
  'AdtProd '[ 'TreeMeta ('NamedField "prInt") Auto Int]

type FlattyMeta =
  'AdtProd '[ 'TreeMeta ('NamedField "flat1") Prim Int, 'TreeMeta ('NamedField "flat2") Prim Text]

type FlattyMetaAuto =
  'AdtProd '[ 'TreeMeta ('NamedField "flat1") Auto Int, 'TreeMeta ('NamedField "flat2") Auto Text]

type PrimDouble name =
  'Kind.Tree ('NamedField name) '[Prim] ('Kind.Prim Double)

type PrimMaybeDouble name =
  'Kind.Tree ('NamedField name) '[ Tycon Maybe Double, Prim] ('Kind.Prim (Maybe Double))

type PrimInt name =
  'Kind.Tree ('NamedField name) '[Prim] ('Kind.Prim Int)

type PrimText name =
  'Kind.Tree ('NamedField name) '[Prim] ('Kind.Prim Text)

type ProddoType name =
  'Kind.Tree ('NamedField name) '[ADT ProddoMeta (Product ProddoRep)] ('Kind.Prod Proddo '[PrimInt "prInt"])

type NewtType =
  'Kind.Tree ('NamedField "newt") [Tycon Maybe Newt, Newtype Newt Text, Prim] ('Kind.Prim (Maybe Newt))

type SummerConssType =
  [
    'Kind.ConUna ('NamedField "Summer1") ('Kind.Tree ('NamedField "txt") '[Prim] ('Kind.Prim Text)),
    'Kind.Con ('NamedField "Summer2") [
      'Kind.Tree ('NamedField "int") '[Prim] ('Kind.Prim Int),
      'Kind.Tree ('NamedField "double") '[Prim] ('Kind.Prim Double)
    ]
  ]

type SummerMeta =
  ADTMeta' (Sum SummerRep) Summer

type DatType =
  'Kind.Tree ('NamedField "Dat") '[ ADT (ADTMeta' (Rep '[Product DatRep]) Dat) (Product DatRep)] (
    'Kind.Prod Dat '[
      PrimMaybeDouble "double"
      ,
      ProddoType "proddo"
      ,
      'Kind.Tree ('NamedField "summer") '[ ADT SummerMeta (Sum SummerRep)] ('Kind.SumProd Summer SummerConssType)
      ,
      'Kind.Tree ('NamedField "custom") '[Prim] ('Kind.Prim Custom)
      ,
      NewtType
      ,
      'Kind.Tree ('NamedField "nummo") '[Tycon [] Nummo, Enum] ('Kind.Prim [Nummo])
      ,
      'Kind.Tree ('NamedField "flatty") '[ADT FlattyMeta (Flatten FlattyRep)] ('Kind.Prod Flatty [
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
  'Kind.Tree ('NamedField "Dat") '[ ADT (ADTMeta' Auto Dat) Auto] (
    'Kind.Prod Dat [
      PrimMaybeDouble "double",
      'Kind.Tree ('NamedField "proddo") '[ ADT ProddoMeta Auto] ProddoTypeAuto,
      'Kind.Tree ('NamedField "summer") '[ ADT SummerMetaAuto Auto] ('Kind.SumProd Summer SummerConssType),
      'Kind.Tree ('NamedField "custom") '[Prim] ('Kind.Prim Custom),
      NewtType,
      'Kind.Tree ('NamedField "nummo") '[Tycon [] Nummo, Enum] ('Kind.Prim [Nummo]),
      'Kind.Tree ('NamedField "flatty") '[ADT FlattyMetaAuto Auto] ('Kind.Prod Flatty [PrimInt "flat1", PrimText "flat2"])
    ]
  )

type PR =
  '[Product ProddoRep]

tree_Int :: TableTree ('Kind.Tree ('NamedField "int") '[Prim] ('Kind.Prim Int))
tree_Int =
  tree @TableParams @('TreeMeta ('NamedField "int") (Rep '[Prim]) Int) mempty

tree_Double :: TableTree (PrimDouble "double")
tree_Double =
  tree @TableParams @('TreeMeta ('NamedField "double") Auto Double) mempty

tree_Newt :: TableTree ('Kind.Tree ('NamedField "newt") '[Newtype Newt Text, Prim] ('Kind.Prim Newt))
tree_Newt =
  tree @TableParams @('TreeMeta ('NamedField "newt") Auto Newt) mempty

tree_Newt_Prim :: TableTree ('Kind.Tree ('NamedField "newt") '[PrimaryKey, Prim] ('Kind.Prim NewtPrim))
tree_Newt_Prim =
  tree @TableParams @('TreeMeta ('NamedField "newt") (Rep '[ForcePrim NewtPrim, PrimaryKey]) NewtPrim) mempty

tree_Proddo :: TableTree (ProddoType "proddo")
tree_Proddo =
  tree @TableParams @('TreeMeta ('NamedField "proddo") (Rep PR) Proddo) mempty

root_Dat_explicit ::
  TableTree DatType
root_Dat_explicit =
  tableRoot @(Product DatRep) @Dat

root_Dat_Auto ::
  TableTree DatTypeAuto
root_Dat_Auto =
  tableRoot @Auto @Dat

effectfulTest ::
  ADTRep (Rep PR) ~ ProddoRep =>
  ADTMeta (Rep PR) Proddo ~ 'MaybeADT ProddoMeta =>
  IsADT (Rep PR) Proddo ('Just ProddoMeta) =>
  MaybeADTResolves (ADTMeta (Rep '[]) Text) 'Nothing =>
  ResolveColumnEffects (Rep '[ForcePrim Newt]) Newt '[Prim] =>
  ResolveColumnEffects Auto (Maybe [Newt]) '[Tycon Maybe [Newt], Tycon [] Newt, Newtype Newt Text, Prim] =>
  ResolveColumnEffects (Rep '[Tycon Maybe [Newt], Tycon [] Newt, Newtype Newt Text]) (Maybe [Newt]) '[Tycon Maybe [Newt], Tycon [] Newt, Newtype Newt Text, Prim] =>
  ResolveColumnEffects Auto Proddo '[ADT ProddoMeta Auto] =>
  ResolveColumnEffects (Rep '[Product Auto]) Proddo '[ADT ProddoMeta (Product Auto)] =>
  ResolveColumnEffects (Rep '[Product ProddoRep]) Proddo '[ADT ProddoMeta (Product ProddoRep)] =>
  ResolveColumnEffects (Rep '[Product ProddoRep]) (Maybe Proddo) '[Tycon Maybe Proddo, ADT ProddoMeta (Product ProddoRep)] =>
  ResolveColumnEffects Auto (Maybe Proddo) '[Tycon Maybe Proddo, ADT ProddoMeta Auto] =>
  ResolveColumnEffects Auto (Maybe [Proddo]) '[Tycon Maybe [Proddo], Tycon [] Proddo, ADT ProddoMeta Auto] =>
  ResolveRep (Rep '[]) ('D Custom) ('Effs '[Prim]) =>
  NewtypeOrADT ('Left PR) ('D Proddo) ('Effs '[ADT ProddoMeta (Product ProddoRep)]) =>
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
    Data.Column "summer" "\"summer\"" "summer" def (Data.Prod [
      Data.Column "sum__index" "(\"summer\").\"sum__index\"" "bigint" def (Data.Prim),
      Data.Column "txt" "(\"summer\").\"txt\"" "text" def Data.Prim,
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
    datTargetAuto === dataTable (tableRoot @Auto @Dat)
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
      tableRoot @(Product DatRep) @Dat
    intTarget =
      Data.Column "int" "\"int\"" "bigint" def Data.Prim
    intCol =
      dataTable (tree @TableParams @('TreeMeta ('NamedField "int") (Prim) Int) mempty)
    proddoTarget =
      Data.Column "proddo" "\"proddo\"" "proddo" def (Data.Prod [
        Data.Column "pr_int" "\"pr_int\"" "bigint" def Data.Prim
      ])
    proddoCol =
      dataTable (tree @TableParams @('TreeMeta ('NamedField "proddo") (Product ProddoRep) Proddo) mempty)
