module Polysemy.Hasql.Test.UnarySumTest where

import Polysemy.Db.Data.Column (Auto, Flatten, Prim, PrimQuery, Product, Sum)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.FieldId (FieldId(NamedField, NumberedField))
import qualified Polysemy.Db.Data.Store as Store
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Meta (ADTMeta')
import Polysemy.Test (UnitTest, assertJust)

import Polysemy.Hasql.Test.Database (withTestStoreGen)
import Polysemy.Hasql.Test.Run (integrationTest)
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

type FlattyMeta =
  ADTMeta' (Flatten Auto) Flatty

type Sum1Meta =
  ADTMeta' (Product Sum1Rep) Sum1

type Sum2Meta =
  ADTMeta' (Product Auto) Sum2

type UnaSumMeta =
  ADTMeta' (Sum UnaSumRep) UnaSum

type UnaSumType =
  'Kind.Tree ('NamedField "UnaSum") '[ADT UnaSumMeta (Sum UnaSumRep)] ('Kind.SumProd UnaSum '[
    'Kind.ConUna ('NamedField "UnaSum1") (
      'Kind.Tree ('NumberedField "UnaSum1" 1) '[ADT Sum1Meta (Product Sum1Rep)] ('Kind.Prod Sum1 '[
        'Kind.Tree ('NamedField "int1") '[Prim] ('Kind.Prim Int),
        'Kind.Tree ('NamedField "flatty") '[ADT FlattyMeta (Flatten Auto)] ('Kind.Prod Flatty '[
          'Kind.Tree ('NamedField "txt") '[Prim] ('Kind.Prim Text),
          'Kind.Tree ('NamedField "double") '[Prim] ('Kind.Prim Double)
        ])
      ])
    ),
    'Kind.ConUna ('NamedField "UnaSum2") (
      'Kind.Tree ('NumberedField "UnaSum2" 1) '[ADT Sum2Meta Auto] ('Kind.Prod Sum2 '[
        'Kind.Tree ('NamedField "int2") '[Prim] ('Kind.Prim Int),
        'Kind.Tree ('NamedField "double") '[Prim] ('Kind.Prim Double)
      ])
    )
  ])

columns_UnaSum_explicit ::
  TableTree UnaSumType
columns_UnaSum_explicit =
  tableRoot @(Sum UnaSumRep) @UnaSum

specimen :: UnaSum
specimen =
  UnaSum1 (Sum1 5 (Flatty "flatty" 1.9))

test_unarySum :: UnitTest
test_unarySum =
  integrationTest do
    result <- withTestStoreGen @(PrimQuery "double") @(Sum UnaSumRep) @Double @UnaSum do
      restop @DbError (Store.upsert specimen)
      restop @DbError (Store.fetch 1.9)
    assertJust specimen result
