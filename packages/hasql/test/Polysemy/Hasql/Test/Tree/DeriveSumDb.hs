{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.Tree.DeriveSumDb where

import Polysemy.Db.Data.Rep (Auto, Prim, PrimQuery, Rep)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree (AdtNode, Node, Tree)
import Polysemy.Db.Tree.Data.TreeMeta (TreeMeta(TreeMeta))
import Polysemy.Test (UnitTest)

import Polysemy.Hasql.Tree.Table (TableParams, TableRoot)
import Polysemy.Hasql.QueryParams (QueryParams)
import Polysemy.Hasql.Table.Schema (Schema)
import Polysemy.Hasql.Table.BasicSchema (BasicSchema)
import Polysemy.Hasql.Test.Tree.Data.DatS (
  DatS,
  DatSAdtMeta,
  DatSNode,
  DatSSum,
  DatSTree,
  DatSTreeEffs,
  )
import Polysemy.Hasql.Where (Where)

type IdQueryTree =
  'Kind.Tree ('NamedField "id") '[Prim] ('Kind.Prim Int)

datSDerivation ::
  d ~ DatS =>
  p ~ TableParams =>
  meta ~ 'TreeMeta ('NamedField "DatS") Auto DatS =>
  node ~ DatSNode =>
  effs ~ DatSTreeEffs =>
  ns ~ DatSSum =>
  AdtNode p d DatSAdtMeta '[] node =>
  Node p ('NamedField "DatS") DatS effs node =>
  Tree p meta DatSTree =>
  Where Auto DatSTree DatS DatSTree DatS =>
  Tree p ('TreeMeta ('NamedField "id") (Rep '[Prim]) Int) IdQueryTree =>
  Where (PrimQuery "int") IdQueryTree Int DatSTree DatS =>
  TableRoot Auto DatS DatSTree =>
  TableRoot (PrimQuery "id") Int IdQueryTree =>
  QueryParams DatSTree DatS =>
  QueryParams IdQueryTree Int =>
  BasicSchema Auto DatS =>
  Schema (PrimQuery "id") Auto Int DatS =>
  ()
datSDerivation =
  ()

test_deriveSumDb :: UnitTest
test_deriveSumDb =
  pure datSDerivation
