{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.Tree.DeriveSum where

import Generics.SOP (K)
import Polysemy.Db.Data.Column (Auto)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import Polysemy.Db.Tree (AdtNode, Node, SumNode, SumTrees, Tree)
import Polysemy.Db.Tree.Data (DataParams)
import Polysemy.Db.Tree.Data.TreeMeta (TreeMeta(TreeMeta))
import Polysemy.Db.Tree.Partial (PartialParams, PartialTag)
import Polysemy.Test (UnitTest)

import Polysemy.Hasql.Test.Tree.Data.DatS (
  DatS,
  DatSAdtMeta,
  DatSAdtMetas,
  DatSDataNode,
  DatSDataTree,
  DatSDataTrees,
  DatSPartialNode,
  DatSPartialTree,
  DatSTreeEffs,
  )

datSDerivation ::
  d ~ DatS =>
  p ~ DataParams =>
  meta ~ 'TreeMeta ('NamedField "DatS") Auto DatS =>
  node ~ DatSDataNode =>
  effs ~ DatSTreeEffs =>
  SumTrees p DatSAdtMetas DatSDataTrees =>
  SumNode p DatS DatSAdtMetas node =>
  AdtNode p d DatSAdtMeta '[] node =>
  Node p ('NamedField "DatS") DatS effs node =>
  Tree p meta DatSDataTree =>
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
  SumNode p DatS DatSAdtMetas node =>
  AdtNode p d DatSAdtMeta '[] node =>
  Node p ('NamedField "DatS") DatS effs node =>
  Tree p meta DatSPartialTree =>
  ()
datSPartialDerivation =
  ()

test_deriveSum :: UnitTest
test_deriveSum = do
  pure datSDerivation
  pure datSPartialDerivation
