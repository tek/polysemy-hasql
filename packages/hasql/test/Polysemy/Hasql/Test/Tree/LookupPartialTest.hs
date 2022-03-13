module Polysemy.Hasql.Test.Tree.LookupPartialTest where

import Polysemy.Db.Kind.Data.Tree (PrimTree, ProdRoot, ProdTree)
import Polysemy.Db.Tree.Lookup (lookupNames)
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

target :: PartialTree Sub1Tree
target =
  partially @Sub1 ++> field @"int" (5 :: Int)

test_lookupPartial :: UnitTest
test_lookupPartial =
  runTestAuto do
    target === lookupNames @["sub", "sub1"] tree
