{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.Partial.DeriveSumDb where

import Polysemy.Db.Data.Column (Auto, Prim, PrimQuery, Rep)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree (AdtTree, Node, Tree)
import Polysemy.Db.Tree.Meta (TreeMeta(TreeMeta))
import Polysemy.Test (UnitTest)

import Polysemy.Hasql.Column.Tree (DbParams, TableColumn)
import Polysemy.Hasql.QueryParams (QueryParams)
import Polysemy.Hasql.Table.QueryTable (GenQuery, GenQueryTable)
import Polysemy.Hasql.Table.Table (GenTable)
import Polysemy.Hasql.Test.Partial.Data.DatS (
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
  p ~ DbParams =>
  meta ~ 'TreeMeta ('NamedField "DatS") Auto DatS =>
  node ~ DatSNode =>
  effs ~ DatSTreeEffs =>
  ns ~ DatSSum =>
  AdtTree p d DatS DatSAdtMeta '[] node =>
  Node p ('NamedField "DatS") DatS DatS effs node =>
  Tree p DatS meta DatSTree =>
  Where DatSTree DatS DatSTree DatS =>
  Tree p Int ('TreeMeta ('NamedField "id") (Rep '[Prim]) Int) IdQueryTree =>
  Where IdQueryTree Int DatSTree DatS =>
  TableColumn Auto DatS DatSTree =>
  TableColumn (PrimQuery "id") Int IdQueryTree =>
  QueryParams DatSTree DatS =>
  QueryParams IdQueryTree Int =>
  GenQuery (PrimQuery "id") Auto Int DatS =>
  GenTable Auto DatS =>
  GenQueryTable (PrimQuery "id") Auto Int DatS =>
  ()
datSDerivation =
  ()

test_deriveSum :: UnitTest
test_deriveSum =
  pure datSDerivation
