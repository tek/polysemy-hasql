module Polysemy.Hasql.Test.SumTableTest where

import Polysemy.Db.Data.Column (Auto, Prim, PrimQuery, Product, Sum)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import qualified Polysemy.Db.Data.Store as Store
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Meta (ADTMeta')
import Polysemy.Test (Hedgehog, UnitTest, assertJust)

import qualified Polysemy.Hasql.Column.Tree as Tree
import Polysemy.Hasql.Column.Tree (tableColumn)
import Polysemy.Hasql.Table.QueryTable (GenQueryTable)
import Polysemy.Hasql.Test.Database (TestStoreDeps, withTestStoreGen)
import Polysemy.Hasql.Test.Run (integrationTest)

data SumTab =
  SumTabOne { id :: Int, text :: Text }
  |
  SumTabTwo { id :: Int, double :: Double }
  deriving (Eq, Show, Generic)

type SumTabMeta =
  ADTMeta' (Product Auto) SumTab

type SumTabType =
  'Kind.Tree ('NamedField "SumTab") '[ADT SumTabMeta (Sum Auto)] ('Kind.SumProd SumTab '[
    'Kind.Con ('NamedField "SumTabOne") '[
      'Kind.Tree ('NamedField "id") '[Prim] ('Kind.Prim Int),
      'Kind.Tree ('NamedField "text") '[Prim] ('Kind.Prim Text)
    ],
    'Kind.Con ('NamedField "SumTabTwo") '[
      'Kind.Tree ('NamedField "id") '[Prim] ('Kind.Prim Int),
      'Kind.Tree ('NamedField "double") '[Prim] ('Kind.Prim Double)
    ]
  ])

columns_SumTab_explicit ::
  Tree.Column SumTabType
columns_SumTab_explicit =
  tableColumn @(Sum Auto) @SumTab

id' :: Int
id' =
  1

specimen :: SumTab
specimen =
  SumTabTwo id' 1.9

prog ::
  Members (Hedgehog IO : TestStoreDeps) r =>
  GenQueryTable (PrimQuery "id") Auto Int SumTab =>
  Sem r ()
prog = do
  result <- withTestStoreGen @(PrimQuery "id") @Auto @Int @SumTab do
    restop (Store.upsert specimen)
    restop (Store.fetch id')
  assertJust specimen result

test_unaryVariants :: UnitTest
test_unaryVariants =
  integrationTest do
    prog
