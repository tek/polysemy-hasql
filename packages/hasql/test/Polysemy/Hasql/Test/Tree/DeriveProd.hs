{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.Tree.DeriveProd where

import Generics.SOP (I(I))
import Polysemy.Db.Data.Column (Auto, Prim)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree (AdtTree, Node, ProdTrees, Tree, TreeProduct)
import Polysemy.Db.Tree.Data (DataParams, DataTag)
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Meta (AdtMetadata(AdtProd))
import Polysemy.Test (UnitTest)

import Polysemy.Hasql.Where (Where)

data Dat =
  Dat {
    int :: Int,
    double :: Double
  }
  deriving (Eq, Show, Generic)

type DatAdtMetas =
  '[
    'TreeMeta ('NamedField "int") Auto Int,
    'TreeMeta ('NamedField "double") Auto Double
  ]

type DatAdtMeta =
  'AdtProd DatAdtMetas

type DatTreeEffs =
  '[ADT DatAdtMeta Auto]

type DatTrees =
  '[
    'Kind.Tree ('NamedField "int") '[Prim] ('Kind.Prim Int),
    'Kind.Tree ('NamedField "double") '[Prim] ('Kind.Prim Double)
  ]

type DatNode =
  'Kind.Prod Dat DatTrees

type DatTree =
  'Kind.Tree ('NamedField "Dat") DatTreeEffs DatNode

datDerivation ::
  d ~ Dat =>
  tag ~ DataTag =>
  p ~ DataParams =>
  f ~ I =>
  meta ~ 'TreeMeta ('NamedField "Dat") Auto Dat =>
  metas ~ DatAdtMetas =>
  node ~ DatNode =>
  effs ~ DatTreeEffs =>
  TreeProduct tag f metas (f d) =>
  ProdTrees p metas DatTrees =>
  AdtTree p d DatAdtMeta '[] node =>
  Node p ('NamedField "Dat") d effs node =>
  Tree p meta DatTree =>
  Where DatTree d DatTree d =>
  ()
datDerivation =
  ()

test_deriveProd :: UnitTest
test_deriveProd =
  pure datDerivation
