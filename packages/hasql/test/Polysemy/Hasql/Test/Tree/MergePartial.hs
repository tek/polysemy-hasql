module Polysemy.Hasql.Test.Tree.MergePartial where

import Polysemy.Db.Data.FieldId (FieldId (NamedField))
import Polysemy.Db.Data.Rep (Auto)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Kind.Data.Tree (AdtRoot, AdtTree, PrimTree, ProdRoot, ProdTree)
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Merge (mergeAt)
import Polysemy.Db.Tree.Meta (ADTMeta')
import Polysemy.Db.Tree.Partial (PartialTree, partially)
import Polysemy.Test (UnitTest, runTestAuto)

data Sub1 =
  Sub1 {
    int :: Int
  }
  deriving (Eq, Show, Generic)

data Sub =
  Sub {
    double :: Double,
    sub1 :: Sub1
  }
  deriving (Eq, Show, Generic)

data Dat =
  Dat {
    sub :: Sub
  }
  deriving (Eq, Show, Generic)

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
  partially @Dat +> field @"int" (5 :: Int) +> field @"double" (2.4 :: Double)

patch :: PartialTree Sub1Tree
patch =
  partially @Sub1 +> field @"int" (11 :: Int)

test_mergePartial :: UnitTest
test_mergePartial =
  runTestAuto do
    dbgs (mergeAt @['NamedField "sub", 'NamedField "sub1"] tree patch)
