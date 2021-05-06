module Polysemy.Hasql.Test.PartialTest where

import Polysemy.Db.Data.Column (Auto, Prim)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree.Data (DataTree, dataTree)
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Meta (ADTMetadata(ADTProd), TreeMeta(TreeMeta))
import Polysemy.Db.Tree.Partial (PartialTree, partial, updatePartial, (...>))
import Polysemy.Test (UnitTest, runTestAuto, (===))
import Polysemy.Hasql.Test.Partial.Data.DatS (DatSPartialTree, DatS (DatS1), DatSDataTree)
import Polysemy.Db.Partial (field)

data Dat =
  Dat {
    int :: Int,
    double :: Double
  }
  deriving (Eq, Show, Generic)

type TreeEffs =
  '[
    ADT ('ADTProd '[
      'TreeMeta ('NamedField "int") Auto Int,
      'TreeMeta ('NamedField "double") Auto Double
    ]) Auto
  ]

type DatNode =
  'Kind.Prod Dat '[
    'Kind.Tree ('NamedField "int") '[Prim] ('Kind.Prim Int),
    'Kind.Tree ('NamedField "double") '[Prim] ('Kind.Prim Double)
  ]

type DatTree =
  'Kind.Tree ('NamedField "Dat") TreeEffs DatNode

datTree :: DataTree DatTree
datTree =
  dataTree @Dat record

record :: Dat
record =
  Dat 9 5

target :: Dat
target =
  Dat 5 17.5

partialUpdateTree :: PartialTree DatTree
partialUpdateTree =
  partial @Dat ...> field @"int" (5 :: Int) ...> field @"double" (17.5 :: Double)

test_partialTree :: UnitTest
test_partialTree =
  runTestAuto do
    target === updatePartial partialUpdateTree record

partialUpdateTreeSum :: PartialTree DatSPartialTree
partialUpdateTreeSum =
  partial @DatS ...> field @"id" (5 :: Int) ...> field @"double1" (17.5 :: Double)

recordS :: DatS
recordS =
  DatS1 9 5

targetS :: DatS
targetS =
  DatS1 5 17.5

datSTree :: DataTree DatSDataTree
datSTree =
  dataTree @DatS recordS

test_partialUpdateSum :: UnitTest
test_partialUpdateSum =
  runTestAuto do
    targetS === updatePartial partialUpdateTreeSum recordS
