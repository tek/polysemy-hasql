{-# options_ghc -fconstraint-solver-iterations=10 #-}

module Polysemy.Hasql.Test.UnarySumTest where

import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.FieldId (FieldId (NamedField, NumberedField))
import qualified Polysemy.Db.Data.QueryStore as QueryStore
import Polysemy.Db.Data.Rep (Auto, Flatten, Prim, PrimaryKey, Product, Sum, UidNestRep)
import Polysemy.Db.Data.Uid (Uid (Uid))
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree.Data.Effect (Adt)
import Polysemy.Db.Tree.Meta (AdtMeta')
import Polysemy.Test (UnitTest, assertJust)

import Polysemy.Hasql.Test.QueryStore (withTestQueryStoreUid)
import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Hasql.Tree.Table (TableTree, tableRoot)

data Flatty =
  Flatty {
    txt :: Text,
    double :: Double
  }
  deriving stock (Eq, Show, Generic)

data Sum1 =
  Sum1 {
    int1 :: Int,
    flatty :: Flatty
  }
  deriving stock (Eq, Show, Generic)

data Sum1Rep =
  Sum1Rep {
    int1 :: Auto,
    flatty :: Flatten Auto
  }
  deriving stock (Eq, Show, Generic)

data Sum2 =
  Sum2 {
    int2 :: Int,
    double :: Double
  }
  deriving stock (Eq, Show, Generic)

data UnaSum =
  UnaSum1 Sum1
  |
  UnaSum2 Sum2
  deriving stock (Eq, Show, Generic)

data UnaSumRep =
  UnaSum1Rep (Product Sum1Rep)
  |
  UnaSum2Rep Auto
  deriving stock (Eq, Show, Generic)

type FlattyMeta =
  AdtMeta' (Flatten Auto) Flatty

type Sum1Meta =
  AdtMeta' (Product Sum1Rep) Sum1

type Sum2Meta =
  AdtMeta' (Product Auto) Sum2

type UnaSumMeta =
  AdtMeta' (Sum UnaSumRep) UnaSum

type UnaSumType =
  'Kind.Tree ('NamedField "payload") '[Adt UnaSumMeta (Sum UnaSumRep)] ('Kind.SumProd UnaSum '[
    'Kind.ConUna 0 ('NamedField "UnaSum1") (
      'Kind.Tree ('NumberedField "UnaSum1" 1) '[Adt Sum1Meta (Product Sum1Rep)] ('Kind.Prod Sum1 '[
        'Kind.Tree ('NamedField "int1") '[Prim] ('Kind.Prim Int),
        'Kind.Tree ('NamedField "flatty") '[Adt FlattyMeta (Flatten Auto)] ('Kind.Prod Flatty '[
          'Kind.Tree ('NamedField "txt") '[Prim] ('Kind.Prim Text),
          'Kind.Tree ('NamedField "double") '[Prim] ('Kind.Prim Double)
        ])
      ])
    ),
    'Kind.ConUna 1 ('NamedField "UnaSum2") (
      'Kind.Tree ('NumberedField "UnaSum2" 1) '[Adt Sum2Meta Auto] ('Kind.Prod Sum2 '[
        'Kind.Tree ('NamedField "int2") '[Prim] ('Kind.Prim Int),
        'Kind.Tree ('NamedField "double") '[Prim] ('Kind.Prim Double)
      ])
    )
  ])


type MainRep =
  UidNestRep PrimaryKey (Sum UnaSumRep)

type UidUnaSumType =
  'Kind.Tree ('NamedField "UnaSum") '[Adt (AdtMeta' (Product MainRep) (Uid Int UnaSum)) (Product MainRep)] (
    'Kind.Prod (Uid Int UnaSum) '[
      'Kind.Tree ('NamedField "id") '[PrimaryKey, Prim] ('Kind.Prim Int),
      UnaSumType
    ]
  )

columns_UnaSum_explicit ::
  TableTree UidUnaSumType
columns_UnaSum_explicit =
  tableRoot @MainRep @(Uid Int UnaSum)

specimen :: UnaSum
specimen =
  UnaSum1 (Sum1 5 (Flatty "flatty" 1.9))

data QP =
  QP { double :: Double }
  deriving stock (Eq, Show, Generic)

data Q =
  Q { payload :: QP }
  deriving stock (Eq, Show, Generic)

data QRep =
  QRep {
    payload :: Product Auto
  }
  deriving stock (Eq, Show, Generic)

test_unarySum :: UnitTest
test_unarySum =
  integrationTest do
    result <- withTestQueryStoreUid @Auto @UnaSumRep @Int do
      restop @DbError do
        QueryStore.upsert (Uid 1 specimen)
        QueryStore.query (QP 1.9)
    assertJust [Uid 1 specimen] result
