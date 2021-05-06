{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.Partial.DeriveSum where

import Polysemy.Db.Data.Column (Auto)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import Polysemy.Db.Tree (AdtTree, Node, SumNode, SumTrees, Tree)
import Polysemy.Db.Tree.Data (DataParams)
import Polysemy.Db.Tree.Meta (TreeMeta(TreeMeta))
import Polysemy.Test (UnitTest)

import Polysemy.Hasql.Test.Partial.Data.DatS (
  DatS,
  DatSAdtMeta,
  DatSAdtMetas,
  DatSDataNode,
  DatSDataTree,
  DatSDataTrees,
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
  SumNode p 'True DatS DatS DatSAdtMetas node =>
  AdtTree p d DatS DatSAdtMeta '[] node =>
  Node p ('NamedField "DatS") DatS DatS effs node =>
  Tree p DatS meta DatSDataTree =>
  Where DatSDataTree DatS DatSDataTree DatS =>
  ()
datSDerivation =
  ()

test_deriveSum :: UnitTest
test_deriveSum =
  pure datSDerivation
