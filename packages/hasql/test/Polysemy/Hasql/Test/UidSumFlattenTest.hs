{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.UidSumFlattenTest where

import qualified Chronos as Chronos
import Polysemy.Db.Data.Rep (Auto, Flatten, Prim, PrimQuery, PrimaryKey, Product, Sum, UidNestRep)
import Polysemy.Db.Data.FieldId (FieldId (NamedField, NumberedField))
import Polysemy.Db.Data.Uid (Uid)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Meta (ADTMeta')
import Polysemy.Test (UnitTest)

import Polysemy.Hasql.Table.Schema (Schema)
import Polysemy.Hasql.Tree.Table (TableTree, tableRoot)

data Flatty =
  Flatty {
    txt :: Text,
    double :: Double
  }
  deriving (Eq, Show, Generic)

data Sum1 =
  Sum1 {
    int1 :: Int,
    flatty :: Flatty
  }
  deriving (Eq, Show, Generic)

data Sum1Rep =
  Sum1Rep {
    int1 :: Auto,
    flatty :: Flatten Auto
  }
  deriving (Eq, Show, Generic)

data Sum2 =
  Sum2 {
    int2 :: Int,
    double :: Double
  }
  deriving (Eq, Show, Generic)

data UnaSum =
  UnaSum1 Sum1
  |
  UnaSum2 Sum2
  deriving (Eq, Show, Generic)

data UnaSumRep =
  UnaSum1Rep (Product Sum1Rep)
  |
  UnaSum2Rep Auto
  deriving (Eq, Show, Generic)

data Wrap d =
  Wrap {
    record :: d,
    time :: Chronos.Time
  }
  deriving (Eq, Show, Generic)

data WrapRep d =
  WrapRep {
    record :: d,
    time :: Prim
  }
  deriving (Eq, Show, Generic)

type FlattyMeta =
  ADTMeta' (Flatten Auto) Flatty

type Sum1Meta =
  ADTMeta' (Product Sum1Rep) Sum1

type Sum2Meta =
  ADTMeta' (Product Auto) Sum2

type UnaSumMeta =
  ADTMeta' (Sum UnaSumRep) UnaSum

type MainRep =
  WrapRep (Sum UnaSumRep)

type MainMeta =
  ADTMeta' (Product MainRep) (Wrap UnaSum)

type UnaSumType =
  'Kind.Tree ('NamedField "record") '[ADT UnaSumMeta (Sum UnaSumRep)] ('Kind.SumProd UnaSum '[
    'Kind.ConUna 0 ('NamedField "UnaSum1") (
      'Kind.Tree ('NumberedField "UnaSum1" 1) '[ADT Sum1Meta (Product Sum1Rep)] ('Kind.Prod Sum1 '[
        'Kind.Tree ('NamedField "int1") '[Prim] ('Kind.Prim Int),
        'Kind.Tree ('NamedField "flatty") '[ADT FlattyMeta (Flatten Auto)] ('Kind.Prod Flatty '[
          'Kind.Tree ('NamedField "txt") '[Prim] ('Kind.Prim Text),
          'Kind.Tree ('NamedField "double") '[Prim] ('Kind.Prim Double)
        ])
      ])
    ),
    'Kind.ConUna 1 ('NamedField "UnaSum2") (
      'Kind.Tree ('NumberedField "UnaSum2" 1) '[ADT Sum2Meta Auto] ('Kind.Prod Sum2 '[
        'Kind.Tree ('NamedField "int2") '[Prim] ('Kind.Prim Int),
        'Kind.Tree ('NamedField "double") '[Prim] ('Kind.Prim Double)
      ])
    )
  ])

type MainType =
  'Kind.Tree ('NamedField "payload") '[ADT MainMeta (Product MainRep)] ('Kind.Prod (Wrap UnaSum) '[
    UnaSumType,
    'Kind.Tree ('NamedField "time") '[Prim] ('Kind.Prim Chronos.Time)
  ])

type UidUnaSumType =
  'Kind.Tree ('NamedField "Wrap") '[ADT (ADTMeta' (Product (UidNestRep PrimaryKey (Product MainRep))) (Uid Int (Wrap UnaSum))) (Product (UidNestRep PrimaryKey (Product MainRep)))] (
    'Kind.Prod (Uid Int (Wrap UnaSum)) '[
      'Kind.Tree ('NamedField "id") '[PrimaryKey, Prim] ('Kind.Prim Int),
      MainType
    ]
  )

type TableMain =
  Uid Int (Wrap UnaSum)

type TableRep =
  UidNestRep PrimaryKey (Product MainRep)

columns_UnaSum_explicit ::
  Schema (PrimQuery "id") TableRep Int TableMain =>
  TableTree UidUnaSumType
columns_UnaSum_explicit =
  tableRoot @(UidNestRep PrimaryKey (Product MainRep)) @(Uid Int (Wrap UnaSum))

test_uidSumFlatten :: UnitTest
test_uidSumFlatten =
  void (pure columns_UnaSum_explicit)
