{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.Tree.DeriveSum where

import Generics.SOP (I(I), K)
import Polysemy.Db.Data.Column (Auto)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import Polysemy.Db.Tree (AdtTree, Node, SumNode, SumTrees, TM(TM), Tree)
import Polysemy.Db.Tree.Data (DataParams)
import Polysemy.Db.Tree.Meta (TreeMeta(TreeMeta))
import Polysemy.Db.Tree.Partial (PartialParams, PartialTag)
import Polysemy.Test (UnitTest)
import Polysemy.Hasql.Column.Tree ()

import Polysemy.Hasql.Test.Tree.Data.DatS (
  DatS,
  DatSAdtMeta,
  DatSAdtMetas,
  DatSDataNode,
  DatSDataTree,
  DatSDataTrees,
  DatSPartialNode,
  DatSPartialTree,
  DatSSum,
  DatSTreeEffs,
  )
import Polysemy.Hasql.Where (Where)

datSDerivation ::
  d ~ DatS =>
  p ~ DataParams =>
  meta ~ 'TreeMeta ('NamedField "DatS") Auto DatS =>
  node ~ DatSDataNode =>
  effs ~ DatSTreeEffs =>
  ns ~ DatSSum =>
  SumTrees p ns DatSAdtMetas DatSDataTrees =>
  SumNode p 'True DatS DatSAdtMetas node =>
  AdtTree p d DatSAdtMeta '[] node =>
  Node p ('NamedField "DatS") DatS effs node =>
  Tree p meta DatSDataTree =>
  Where DatSDataTree DatS DatSDataTree DatS =>
  ()
datSDerivation =
  ()

datSPartialDerivation ::
  d ~ DatS =>
  tag ~ PartialTag =>
  f ~ K () =>
  p ~ PartialParams =>
  meta ~ 'TreeMeta ('NamedField "DatS") Auto DatS =>
  metas ~ DatSAdtMetas =>
  node ~ DatSPartialNode =>
  effs ~ DatSTreeEffs =>
  SumNode p 'False DatS DatSAdtMetas node =>
  AdtTree p d DatSAdtMeta '[] node =>
  Node p ('NamedField "DatS") DatS effs node =>
  Tree p meta DatSPartialTree =>
  ()
datSPartialDerivation =
  ()

test_deriveSum :: UnitTest
test_deriveSum = do
  pure datSDerivation
  pure datSPartialDerivation
