{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.Tree.DeriveSumFieldDbTest where

import Polysemy.Db.Data.FieldId (FieldId (NamedField))
import Polysemy.Db.Data.Rep (Auto, Prim)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree (Tree)
import Polysemy.Db.Tree.Data.Effect (Adt)
import Polysemy.Db.Tree.Data.TreeMeta (ConMeta (ConMeta), TreeMeta (TreeMeta))
import Polysemy.Db.Tree.Effect (TreeEffectsFor)
import Polysemy.Db.Tree.Meta (AdtMeta, AdtMetadata (AdtProd, AdtSum), MaybeAdt (MaybeAdt))
import Polysemy.Test (UnitTest)

import Polysemy.Hasql.QueryParams (QueryParams)
import Polysemy.Hasql.QueryRows (QueryRows)
import Polysemy.Hasql.Table.BasicSchema (BasicSchema)
import Polysemy.Hasql.Tree.Table (DbQueryRoot, DbTag, TableParams, TableRoot)
import Polysemy.Hasql.Where (Where)

data Summy =
  Lefty { intL :: Int, doubleL :: Double }
  |
  Righty { intR :: Int, doubleR :: Double }
  deriving stock (Eq, Show, Generic)

data DatSF =
  DatSF {
    id :: Int,
    summy :: Summy
  }
  deriving stock (Eq, Show, Generic)

type DatSFSum =
  [
    [Int, Double],
    [Int, Text]
  ]

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
  Adt DatSFMeta Auto

type DatSFTrees =
  '[
    'Kind.Tree ('NamedField "id") '[Prim] ('Kind.Prim Int),
    'Kind.Tree ('NamedField "summy") '[Adt SummyMeta Auto] ('Kind.SumProd Summy SummyCons)
  ]

type DatSFTree =
  'Kind.Tree ('NamedField "DatSF") '[DatSFEffs] ('Kind.Prod DatSF DatSFTrees)

data Q =
  Q {
    id :: Int
  }
  deriving stock (Eq, Show, Generic)

type QEffs =
  Adt ('AdtProd '[ 'TreeMeta ('NamedField "id") Auto Int]) Auto

type QTree =
  'Kind.Tree ('NamedField "Q") '[QEffs] ('Kind.Prod Q '[ 'Kind.Tree ('NamedField "id") '[Prim] ('Kind.Prim Int)])

type IdQueryTree =
  'Kind.Tree ('NamedField "id") '[Prim] ('Kind.Prim Int)

datSDerivation ::
  p ~ TableParams =>
  d ~ DatSF =>
  'MaybeAdt DatSFMeta ~ AdtMeta Auto DatSF =>
  meta ~ 'TreeMeta ('NamedField "DatSF") Auto d =>
  Tree p meta DatSFTree =>
  Where Auto QTree Q DatSFTree DatSF =>
  TreeEffectsFor DbTag Prim Int '[Prim] =>
  Tree p ('TreeMeta ('NamedField "id") Auto Int) IdQueryTree =>
  TableRoot Auto DatSF DatSFTree =>
  QueryRows DatSFTree DatSF =>
  QueryParams DatSFTree DatSF =>
  BasicSchema Auto DatSF =>
  DbQueryRoot Auto Q DatSF QTree =>
  DbQueryRoot Prim Int DatSF IdQueryTree =>
  -- Where Prim IdQueryTree Int DatSFTree DatSF =>
  -- Schema Prim Auto Int DatSF =>
  ()
datSDerivation =
  ()

test_deriveSumFieldDb :: UnitTest
test_deriveSumFieldDb =
  pure datSDerivation
