module Polysemy.Hasql.Test.PartialTest where

import Polysemy.Db.Data.FieldId (FieldId (NamedField))
import Polysemy.Db.Data.Partial (Partial, partial)
import Polysemy.Db.Data.Rep (Auto, Prim)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree.Data (DataTree, dataTree)
import Polysemy.Db.Tree.Data.Effect (Adt)
import Polysemy.Db.Tree.Data.TreeMeta (TreeMeta (TreeMeta))
import Polysemy.Db.Tree.Meta (AdtMetadata (AdtProd))
import Polysemy.Db.Tree.Partial (PartialTree, field, partially, patch, updatePartial, (++>), (+>))
import Polysemy.Test (UnitTest, runTestAuto, (===))

import Polysemy.Hasql.Test.Tree.Data.DatS (DatS (DatS1), DatSDataTree, DatSPartialTree)

data Dat =
  Dat {
    int :: Int,
    double :: Double
  }
  deriving stock (Eq, Show, Generic)

type TreeEffs =
  '[
    Adt ('AdtProd '[
      'TreeMeta ('NamedField "int") Auto Int,
      'TreeMeta ('NamedField "double") Auto Double
    ]) Auto
  ]

type DatNode =
  'Kind.Prod Dat [
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
  partially @Dat ++> field @"int" (5 :: Int) ++> field @"double" (17.5 :: Double)

test_partialTree :: UnitTest
test_partialTree =
  runTestAuto do
    target === updatePartial partialUpdateTree record

partialUpdate :: Partial Dat
partialUpdate =
  partial @Dat +> field @"int" (5 :: Int) +> field @"double" (17.5 :: Double)

test_partial :: UnitTest
test_partial =
  runTestAuto do
    target === patch partialUpdate record

partialUpdateTreeSum :: PartialTree DatSPartialTree
partialUpdateTreeSum =
  partially @DatS ++> field @"id" (5 :: Int) ++> field @"double1" (17.5 :: Double)

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

data DatN2 =
  DatN2a {
    int :: Int,
    text :: Text
  }
  |
  DatN2b {
    int2 :: Int,
    text :: Text
  }
  deriving stock (Eq, Show, Generic)

data DatN1 =
  DatN1a { n2 :: DatN2, text :: Text }
  |
  DatN1b { n2 :: DatN2, n2b :: DatN2, text :: Text }
  deriving stock (Eq, Show, Generic)

data DatN =
  DatN {
    int :: Int,
    n1 :: DatN1
  }
  deriving stock (Eq, Show, Generic)

recordN :: DatN
recordN =
  DatN 9 (DatN1b (DatN2a 12 "datn2a") (DatN2b 43 "datn2b") "datn1b")

targetN :: DatN
targetN =
  DatN 5 (DatN1b (DatN2a 5 "updated") (DatN2a 101 "updated 101") "updated")

test_partialUpdateNestedSum :: UnitTest
test_partialUpdateNestedSum =
  runTestAuto do
    targetN === updatePartial updateTree recordN
  where
    updateTree =
      partially @DatN
        ++> field @"int" (5 :: Int)
        ++> field @"text" ("updated" :: Text)
        ++> field @"n2b" (DatN2a 101 "updated 101")
