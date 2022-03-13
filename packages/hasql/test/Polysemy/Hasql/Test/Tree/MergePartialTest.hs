module Polysemy.Hasql.Test.Tree.MergePartialTest where

import Polysemy.Db.Kind.Data.Tree (PrimTree, ProdRoot, ProdTree)
import Polysemy.Db.Tree.Merge (mergeAtNames)
import Polysemy.Db.Tree.Partial (PartialTree, field, partially, (++>))
import Polysemy.Test (UnitTest, runTestAuto, (===))

data Sub1 =
  Sub1 {
    int :: Int
  }
  deriving stock (Eq, Show, Generic)

data Sub =
  Sub {
    double :: Double,
    sub1 :: Sub1
  }
  deriving stock (Eq, Show, Generic)

data Dat =
  Dat {
    sub :: Sub
  }
  deriving stock (Eq, Show, Generic)

type Sub1Tree =
  ProdTree "Sub1" Sub1 '[
    PrimTree "int" Int
  ]

type DatTree =
  ProdRoot Dat '[
    ProdTree "sub" Sub '[
      PrimTree "double" Double,
      ProdTree "sub1" Sub1 '[
        PrimTree "int" Int
      ]
    ]
  ]

tree :: PartialTree DatTree
tree =
  partially @Dat ++> field @"int" (5 :: Int) ++> field @"double" (2.4 :: Double)

target :: PartialTree DatTree
target =
  tree ++> field @"int" (11 :: Int)

patch :: PartialTree Sub1Tree
patch =
  partially @Sub1 ++> field @"int" (11 :: Int)

test_mergePartial :: UnitTest
test_mergePartial =
  runTestAuto do
    target === mergeAtNames @["sub", "sub1"] patch tree
