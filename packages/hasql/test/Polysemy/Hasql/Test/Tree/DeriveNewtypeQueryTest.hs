{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.Tree.DeriveNewtypeQueryTest where

import Polysemy.Db.Data.FieldId (FieldId (NamedField))
import Polysemy.Db.Data.Rep (Auto, PrimQuery, PrimaryKey, UidRep)
import Polysemy.Db.Data.Uid (Uid)
import Polysemy.Db.Tree.Data.TreeMeta (TreeMeta (TreeMeta))
import Polysemy.Test (UnitTest)

import Polysemy.Hasql.QueryParams (QueryParams)
import Polysemy.Hasql.Table.Schema (Schema)
import Polysemy.Hasql.Tree.Table (DbQueryRoot, TableParams, TableRoot)
import Polysemy.Hasql.Tree.Value (DbValueQueryTree)
import Polysemy.Hasql.Where (Where)
import Polysemy.Hasql.Where.Dynamic (DynamicQuery)

newtype NtId =
  NtId { id :: Int }
  deriving stock (Eq, Show, Generic)

data Dat =
  Dat {
    number :: Int
  }
  deriving stock (Eq, Show, Generic)

newtypeQueryDerivation ::
  q ~ NtId =>
  qrep ~ PrimQuery "number" =>
  d ~ Dat =>
  p ~ TableParams =>
  meta ~ 'TreeMeta ('NamedField "Dat") Auto Dat =>
  TableRoot Auto d dTree =>
  DbQueryRoot qrep q d qTree =>
  QueryParams qTree q =>
  DbValueQueryTree qrep q d qVTree =>
  DynamicQuery qrep q d =>
  Where qrep qTree q dTree d =>
  Schema (PrimQuery "number") Auto NtId Dat =>
  Schema (PrimQuery "id") (UidRep PrimaryKey Auto) NtId (Uid Int Dat) =>
  Schema Auto Auto NtId Dat =>
  ()
newtypeQueryDerivation =
  ()

test_deriveNewtypeQuery :: UnitTest
test_deriveNewtypeQuery =
  pure newtypeQueryDerivation
