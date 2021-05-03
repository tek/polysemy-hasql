{-# options_ghc -Wno-all -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.PartialTest where

import Polysemy.Db.Data.Column (Auto, Prim, Product, Rep)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import Polysemy.Db.Data.PartialField (PartialField)
import Polysemy.Db.Data.PartialFields (PartialFields)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Partial (field, partial, updatePartial, (.>))
import Polysemy.Db.Tree (Node, Params(Params), Root, TableMeta, Tree)
import Polysemy.Db.Tree.Data (DataTag, DataTree, dataTree)
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Effect (ResolveTreeEffects)
import Polysemy.Db.Tree.Meta (ADTMetadata(ADTProd), TreeMeta(TreeMeta))
import Polysemy.Db.Tree.Partial (PartialTag, PartialTree, partialTree', updatePartialTree', (...>))
import qualified Polysemy.Db.Type.Data.Tree as Type
import Polysemy.Test (UnitTest, runTestAuto, (===))

data Dat =
  Dat {
    int :: Int,
    double :: Double
  }
  deriving (Eq, Show, Generic)

partialUpdate :: PartialFields Dat
partialUpdate =
  partial @Dat .> field @"int" 5 .> field @"double" 17.5

test_partialUpdate :: UnitTest
test_partialUpdate =
  runTestAuto do
    target === updatePartial partialUpdate record

type TreeEffs =
  '[
    ADT ('ADTProd '[
      'TreeMeta ('NamedField "int") Auto Int,
      'TreeMeta ('NamedField "double") Auto Double
    ]) (Product Auto)
  ]

type DatNode =
  'Kind.Prod Dat '[
    'Kind.Tree ('NamedField "int") '[Prim] ('Kind.Prim Int),
    'Kind.Tree ('NamedField "double") '[Prim] ('Kind.Prim Double)
  ]

type TreeType =
  'Kind.Tree ('NamedField "Dat") TreeEffs DatNode

type DataType =
  'Kind.Tree ('NamedField "Dat") TreeEffs ('Kind.Prod Dat '[
    'Kind.Tree ('NamedField "int") '[Prim] ('Kind.Prim Int),
    'Kind.Tree ('NamedField "double") '[Prim] ('Kind.Prim Double)
  ])

datTree :: DataTree DataType
datTree =
  dataTree @Dat record

record :: Dat
record =
  Dat 9 5

target :: Dat
target =
  Dat 5 17.5

partialUpdateTree' :: PartialTree TreeType
partialUpdateTree' =
  partialTree' @Dat ...> field @"int" (5 :: Int) ...> field @"double" (17.5 :: Double)

test_partialTree :: UnitTest
test_partialTree =
  runTestAuto do
    target === updatePartialTree' partialUpdateTree' record
