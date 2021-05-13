{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.Tree.DeriveSumFieldDb where

import Polysemy.Db.Data.Column (Auto, Prim)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import Polysemy.Db.Data.IdQuery (IdQuery)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree (SumIndex, Tree)
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Data.TreeMeta (ConMeta(ConMeta), TreeMeta(TreeMeta))
import Polysemy.Db.Tree.Meta (ADTMeta, AdtMetadata (AdtSum, AdtProd), MaybeADT(MaybeADT))
import Polysemy.Test (UnitTest)

import Polysemy.Hasql.Column.Tree (DbParams, TableColumn)
import Polysemy.Hasql.QueryParams (QueryParams)
import Polysemy.Hasql.QueryRows (QueryRows)
import Polysemy.Hasql.Table.QueryTable (GenQueryTable)
import Polysemy.Hasql.Table.Table (GenTable)
import Polysemy.Hasql.Where (Where)

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
  'Kind.Prod () '[
    'Kind.Tree ('NamedField "intL") '[Prim] ('Kind.Prim Int),
    'Kind.Tree ('NamedField "doubleL") '[Prim] ('Kind.Prim Double)
    ]

type RightyNode =
  'Kind.Prod () '[
    'Kind.Tree ('NamedField "intR") '[Prim] ('Kind.Prim Int),
    'Kind.Tree ('NamedField "doubleR") '[Prim] ('Kind.Prim Double)
    ]

type SummyTree =
  '[
    SumIndex,
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
    'Kind.Tree ('NamedField "summy") '[ADT SummyMeta Auto] ('Kind.Prod Summy SummyTree)
  ]

type DatSFTree =
  'Kind.Tree ('NamedField "DatSF") '[DatSFEffs] ('Kind.Prod DatSF DatSFTrees)

data Q =
  Q {
    id :: Int
  }
  deriving (Eq, Show, Generic)

type QEffs =
  ADT ('AdtProd '[ 'TreeMeta ('NamedField "id") Auto Int]) Auto

type QTree =
  'Kind.Tree ('NamedField "Q") '[QEffs] ('Kind.Prod Q '[ 'Kind.Tree ('NamedField "id") '[Prim] ('Kind.Prim Int)])

type IdQueryEffs =
  ADT ('AdtProd '[ 'TreeMeta ('NamedField "id") Auto Int]) Auto

type IdQueryTree =
  'Kind.Tree ('NamedField "IdQuery") '[IdQueryEffs] ('Kind.Prod (IdQuery Int) '[
    'Kind.Tree ('NamedField "id") '[Prim] ('Kind.Prim Int)
  ])

datSDerivation ::
  p ~ DbParams =>
  d ~ DatSF =>
  'MaybeADT DatSFMeta ~ ADTMeta Auto DatSF =>
  meta ~ 'TreeMeta ('NamedField "DatSF") Auto d =>
  Tree p meta DatSFTree =>
  Where QTree Q DatSFTree DatSF =>
  Tree p ('TreeMeta ('NamedField "IdQuery") Auto (IdQuery Int)) IdQueryTree =>
  TableColumn Auto DatSF DatSFTree =>
  QueryRows DatSFTree DatSF =>
  QueryParams DatSFTree DatSF =>
  GenTable Auto DatSF =>
  GenQueryTable Auto Auto (IdQuery Int) DatSF =>
  ()
datSDerivation =
  ()

test_deriveSumFieldDb :: UnitTest
test_deriveSumFieldDb =
  pure datSDerivation
