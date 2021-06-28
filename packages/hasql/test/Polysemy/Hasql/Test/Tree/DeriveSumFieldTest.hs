{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.Tree.DeriveSumFieldTest where

import Polysemy.Db.Data.Rep (Auto, Prim)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree (Tree)
import Polysemy.Db.Tree.Data (DataParams)
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Data.TreeMeta (ConMeta(ConMeta), TreeMeta(TreeMeta))
import Polysemy.Db.Tree.Meta (ADTMeta, AdtMetadata (AdtSum, AdtProd), MaybeADT(MaybeADT))
import Polysemy.Test (UnitTest)

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
    'ConMeta 0 ('NamedField "DatS1") '[
    'TreeMeta ('NamedField "int1") Auto Int,
    'TreeMeta ('NamedField "double1") Auto Double
  ]


type DatSFAdtMetas =
  '[
    DatSFAdtMeta1,
    'ConMeta 1 ('NamedField "DatS2") '[
      'TreeMeta ('NamedField "int2") Auto Int,
      'TreeMeta ('NamedField "text2") Auto Text
    ]
  ]

type SummyAdtMeta =
  'AdtSum DatSFAdtMetas

type SummyMeta =
  'AdtSum '[
    'ConMeta 0 ('NamedField "Lefty") '[
      'TreeMeta ('NamedField "intL") Auto Int,
      'TreeMeta ('NamedField "doubleL") Auto Double
    ],
    'ConMeta 1 ('NamedField "Righty") '[
      'TreeMeta ('NamedField "intR") Auto Int,
      'TreeMeta ('NamedField "doubleR") Auto Double
    ]
  ]

type Tr = 'Kind.Tree ('NamedField "dummy") '[Prim] ('Kind.Prim Int)

type LeftyTrees =
  '[
    'Kind.Tree ('NamedField "intL") '[Prim] ('Kind.Prim Int),
    'Kind.Tree ('NamedField "doubleL") '[Prim] ('Kind.Prim Double)
  ]

type RightyTrees =
  '[
    'Kind.Tree ('NamedField "intR") '[Prim] ('Kind.Prim Int),
    'Kind.Tree ('NamedField "doubleR") '[Prim] ('Kind.Prim Double)
  ]

type SummyCons =
  '[
    'Kind.Con 0 ('NamedField "Lefty") LeftyTrees,
    'Kind.Con 1 ('NamedField "Righty") RightyTrees
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
    'Kind.Tree ('NamedField "summy") '[ADT SummyMeta Auto] ('Kind.Sum Summy SummyCons)
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
  Tree p meta DatSDataTree =>
  ()
datSDerivation =
  ()

test_deriveSumField :: UnitTest
test_deriveSumField =
  pure datSDerivation
