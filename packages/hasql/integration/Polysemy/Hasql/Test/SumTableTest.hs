module Polysemy.Hasql.Test.SumTableTest where

import Polysemy.Db.Data.Rep (Auto, Prim, PrimQuery, Product, Sum)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.FieldId (FieldId (NamedField))
import qualified Polysemy.Db.Data.Store as Store
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Meta (ADTMeta')
import Polysemy.Test (UnitTest, assertJust)

import Polysemy.Hasql.Test.Database (withTestStoreGen)
import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Hasql.Tree.Table (TableTree, tableRoot)

data SumTab =
  SumTabOne { id :: Int, text :: Text }
  |
  SumTabTwo { id :: Int, double :: Double }
  deriving (Eq, Show, Generic)

type SumTabMeta =
  ADTMeta' (Product Auto) SumTab

type SumTabType =
  'Kind.Tree ('NamedField "SumTab") '[ADT SumTabMeta (Sum Auto)] ('Kind.SumProd SumTab '[
    'Kind.Con 0 ('NamedField "SumTabOne") '[
      'Kind.Tree ('NamedField "id") '[Prim] ('Kind.Prim Int),
      'Kind.Tree ('NamedField "text") '[Prim] ('Kind.Prim Text)
    ],
    'Kind.Con 1 ('NamedField "SumTabTwo") '[
      'Kind.Tree ('NamedField "id") '[Prim] ('Kind.Prim Int),
      'Kind.Tree ('NamedField "double") '[Prim] ('Kind.Prim Double)
    ]
  ])

columns_SumTab_explicit ::
  TableTree SumTabType
columns_SumTab_explicit =
  tableRoot @(Sum Auto) @SumTab

id' :: Int
id' =
  1

specimen :: SumTab
specimen =
  SumTabTwo id' 1.9

test_sumTable :: UnitTest
test_sumTable =
  integrationTest do
    result <- withTestStoreGen @(PrimQuery "id") @Auto @Int @SumTab do
      restop @DbError (Store.upsert specimen)
      restop @DbError (Store.fetch id')
    assertJust specimen result
