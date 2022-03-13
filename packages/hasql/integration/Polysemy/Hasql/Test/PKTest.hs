{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.PKTest where

import Exon (exon)
import Polysemy.Db.Data.ColumnOptions (primaryKey)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.FieldId (FieldId (NamedField))
import Polysemy.Db.Data.Rep (Auto, Flatten, Prim, PrimQuery, PrimaryKey, Product, UidRep)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import Polysemy.Db.Data.Uid (Uid (Uid))
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import qualified Polysemy.Db.Tree as Tree
import Polysemy.Db.Tree.Data.Effect (Adt, Newtype)
import Polysemy.Db.Tree.Data.TreeMeta (TreeMeta (TreeMeta))
import Polysemy.Db.Tree.Effect (TreeEffects)
import Polysemy.Db.Tree.Meta (AdtMeta')
import Polysemy.Test (UnitTest, assertJust, evalEither, (===))

import qualified Polysemy.Hasql.Data.DbType as Data
import Polysemy.Hasql.Data.DbType (TypeName (CompositeTypeName))
import Polysemy.Hasql.Data.QueryTable (QueryTable)
import Polysemy.Hasql.QueryParams (QueryParams)
import Polysemy.Hasql.Table.BasicSchema (BasicSchema)
import Polysemy.Hasql.Table.DataColumn (tableStructure)
import Polysemy.Hasql.Table.Schema (Schema, UidQuerySchema, schema)
import Polysemy.Hasql.Test.Database (withTestStoreGenAs)
import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Hasql.Tree.Table (DbQueryRoot, DbTag, TableParams, TableRoot)
import Polysemy.Hasql.Where (Where)

newtype Id =
  Id { unId :: Int }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

data Rec =
  Rec {
    a :: Int,
    b :: Text
  }
  deriving stock (Eq, Show, Generic)

prog ::
  Member (Store Id Rec) r =>
  Uid Id Rec ->
  Sem r (Either DbError (Maybe (Uid Id Rec)))
prog specimen = do
  runError do
    Store.upsert specimen
    Store.fetch 2

struct :: Data.Column
struct =
  tableStructure @(UidRep PrimaryKey Auto) @(Uid Id Rec)

table :: QueryTable Id (Uid Id Rec)
table =
  schema @(PrimQuery "id") @Auto

type RecType =
  'Kind.Tree ('NamedField "Rec") '[Adt (AdtMeta' (Product (UidRep PrimaryKey Auto)) (Uid Id Rec)) (Product (UidRep PrimaryKey Auto))] ('Kind.Prod (Uid Id Rec) '[
    'Kind.Tree ('NamedField "id") '[Newtype Id Int, PrimaryKey, Prim] ('Kind.Prim Id),
    'Kind.Tree ('NamedField "payload") '[Adt (AdtMeta' (Flatten Auto) Rec) (Flatten Auto)] (
      'Kind.Prod Rec '[
        'Kind.Tree ('NamedField "a") '[Prim] ('Kind.Prim Int),
        'Kind.Tree ('NamedField "b") '[Prim] ('Kind.Prim Text)
      ]
    )
  ])

testDerivation ::
  qrep ~ PrimQuery "id" =>
  irep ~ PrimaryKey =>
  rep ~ Auto =>
  i ~ Id =>
  q ~ Id =>
  d ~ Rec =>
  qTree ~ 'Kind.Tree ('NamedField "id") '[Newtype Id Int, PrimQuery "id", Prim] ('Kind.Prim Id) =>
  Tree.Tree TableParams ('TreeMeta ('NamedField "Rec") (Product (UidRep PrimaryKey Auto)) (Uid Id Rec)) RecType =>
  TableRoot (UidRep irep rep) (Uid i d) dTree =>
  TreeEffects DbTag qrep q '[Newtype Id Int, PrimQuery "id", Prim] =>
  DbQueryRoot qrep i (Uid i d) qTree =>
  BasicSchema (UidRep irep rep) (Uid i d) =>
  QueryParams qTree q =>
  Where qrep qTree q dTree (Uid i d) =>
  Schema qrep (UidRep irep rep) i (Uid i d) =>
  UidQuerySchema qrep irep rep i i d =>
  ()
testDerivation =
  ()

targetStructure :: Data.Column
targetStructure =
  Data.Column "rec" [exon|"rec"|] (CompositeTypeName "uid") def $ Data.Prod [
    Data.Column "id" [exon|"id"|] "bigint" def { primaryKey = True } Data.Prim,
    Data.Column "a" [exon|"a"|] "bigint" def Data.Prim,
    Data.Column "b" [exon|"b"|] "text" def Data.Prim
  ]

test_pk :: UnitTest
test_pk =
  integrationTest do
    pure testDerivation
    targetStructure === struct
    result <- withTestStoreGenAs @(PrimQuery "id") @PrimaryKey @Auto @Id @Rec (restop @DbError (prog specimen))
    assertJust specimen =<< evalEither result
  where
    specimen =
      Uid 2 (Rec 5 "foo")
