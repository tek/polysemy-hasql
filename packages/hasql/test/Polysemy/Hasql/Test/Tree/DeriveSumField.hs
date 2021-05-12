{-# options_ghc -Wno-all -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.Tree.DeriveSumField where

import Polysemy.Db.Tree.Data (DataParams)
import Polysemy.Db.Tree.Meta (AdtMetadata (AdtSum, AdtProd), ADTMeta, MaybeADT(MaybeADT))
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import Polysemy.Db.Data.Column (Auto, Prim, Con)
import Polysemy.Db.Tree (SumConTree, SumTrees, AdtTree, Tree, Node, SumNode)
import Polysemy.Test (UnitTest)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree.Data.Effect (ADT)

data Summy =
  Lefty { intL :: Int, doubleL :: Double }
  |
  Righty { intR :: Int, doubleR :: Double }
  deriving (Eq, Show, Generic)

data DatSF =
  DatSF {
    id :: Int,
    summy :: Summy
  }
  deriving (Eq, Show, Generic)

type DatSFSum =
  [
    [Int, Double],
    [Int, Text]
  ]

type DatSFAdtMeta1 =
    'ConMeta ('NamedField "DatS1") '[
    'TreeMeta ('NamedField "int1") Auto Int,
    'TreeMeta ('NamedField "double1") Auto Double
  ]


type DatSFAdtMetas =
  '[
    DatSFAdtMeta1,
    'ConMeta ('NamedField "DatS2") '[
      'TreeMeta ('NamedField "int2") Auto Int,
      'TreeMeta ('NamedField "text2") Auto Text
    ]
  ]

type SummyAdtMeta =
  'AdtSum DatSFAdtMetas

type SummyMeta =
  'AdtSum '[
    'ConMeta ('NamedField "Lefty") '[
      'TreeMeta ('NamedField "intL") Auto Int,
      'TreeMeta ('NamedField "doubleL") Auto Double
    ],
    'ConMeta ('NamedField "Righty") '[
      'TreeMeta ('NamedField "intR") Auto Int,
      'TreeMeta ('NamedField "doubleR") Auto Double
    ]
  ]

type Tr = 'Kind.Tree ('NamedField "dummy") '[Prim] ('Kind.Prim Int)

type LeftyNode =
  'Kind.Prod (Con ('NamedField "Lefty")) '[
    'Kind.Tree ('NamedField "intL") '[Prim] ('Kind.Prim Int),
    'Kind.Tree ('NamedField "doubleL") '[Prim] ('Kind.Prim Double)
    ]

type RightyNode =
  'Kind.Prod (Con ('NamedField "Righty")) '[
    'Kind.Tree ('NamedField "intR") '[Prim] ('Kind.Prim Int),
    'Kind.Tree ('NamedField "doubleR") '[Prim] ('Kind.Prim Double)
    ]

type SummyTree =
  '[
    'Kind.Tree ('NamedField "Lefty") '[] LeftyNode,
    'Kind.Tree ('NamedField "Righty") '[] RightyNode
  ]

type DatSFMeta =
  'AdtProd '[
    'TreeMeta ('NamedField "id") Auto Int,
    'TreeMeta ('NamedField "summy") Auto Summy
  ]

type DatSFEffs =
  ADT DatSFMeta Auto

type DatSFTrees =
  '[
    'Kind.Tree ('NamedField "id") '[Prim] ('Kind.Prim Int),
    'Kind.Tree ('NamedField "summy") '[ADT SummyMeta Auto] ('Kind.Sum Summy SummyTree)
  ]

type DatSFDataNode =
  'Kind.Prod DatSF DatSFTrees

type DatSDataTree =
  'Kind.Tree ('NamedField "DatSF") '[DatSFEffs] DatSFDataNode

datSDerivation ::
  p ~ DataParams =>
  d ~ DatSF =>
  'MaybeADT DatSFMeta ~ ADTMeta Auto DatSF =>
  meta ~ 'TreeMeta ('NamedField "DatSF") Auto d =>
  Tree p d meta DatSDataTree =>
  ()
datSDerivation =
  ()

test_deriveSumField :: UnitTest
test_deriveSumField =
  pure datSDerivation
