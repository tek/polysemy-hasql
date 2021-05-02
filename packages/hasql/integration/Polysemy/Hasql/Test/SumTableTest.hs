module Polysemy.Hasql.Test.SumTableTest where

import Polysemy.Db.Data.Column (Auto, Con, Prim, PrimQuery, Product, Sum)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Test (Hedgehog, assertJust)

import Polysemy.Hasql.Column.Class (SumIndexColumn, tableColumn)
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Meta (ADTMeta')
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Hasql.Table.QueryTable (GenQueryTable)
import Polysemy.Hasql.Test.Database (TestStoreDeps, withTestStoreGen)
import Polysemy.Hasql.Test.Run (integrationTest)
import qualified Polysemy.Hasql.Type.Data.DbType as Type
import Polysemy.Test (UnitTest)

data SumTab =
  SumTabOne { id :: Int, text :: Text }
  |
  SumTabTwo { id :: Int, double :: Double }
  deriving (Eq, Show, Generic)

type SumTabMeta =
  ADTMeta' (Product Auto) SumTab

type SumTabType =
  'Kind.Tree ('NamedField "SumTab") '[ADT SumTabMeta (Sum Auto)] ('Kind.Sum SumTab '[
    SumIndexColumn,
    'Kind.Tree ('NamedField "SumTabOne") '[] ('Kind.Prod (Con ('NamedField "SumTabOne")) '[
      'Kind.Tree ('NamedField "id") '[Prim] ('Kind.Prim Int),
      'Kind.Tree ('NamedField "text") '[Prim] ('Kind.Prim Text)
    ]),
    'Kind.Tree ('NamedField "SumTabTwo") '[] ('Kind.Prod (Con ('NamedField "SumTabTwo")) '[
      'Kind.Tree ('NamedField "id") '[Prim] ('Kind.Prim Int),
      'Kind.Tree ('NamedField "double") '[Prim] ('Kind.Prim Double)
    ])
  ])

columns_SumTab_explicit ::
  Type.Column SumTabType
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
