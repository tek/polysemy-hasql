{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.PKTest where

import Polysemy.Db.Data.Column (Auto, Flatten, Prim, PrimaryKey, Product, UidRep)
import Polysemy.Db.Data.ColumnOptions (primaryKey)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import Polysemy.Db.Data.IdQuery (IdQuery(IdQuery))
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import Polysemy.Db.Data.Uid (Uid(Uid))
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import qualified Polysemy.Db.Tree as Tree
import Polysemy.Db.Tree.Data.Effect (ADT, Newtype)
import Polysemy.Db.Tree.Data.TreeMeta (TreeMeta(TreeMeta))
import Polysemy.Db.Tree.Meta (ADTMeta')
import Polysemy.Test (UnitTest, assertJust, evalEither, (===))

import Polysemy.Hasql.Column.DataColumn (tableStructure)
import Polysemy.Hasql.Column.Tree (DbParams)
import qualified Polysemy.Hasql.Data.DbType as Data
import Polysemy.Hasql.Data.QueryTable (QueryTable)
import Polysemy.Hasql.Table.QueryTable (queryTable)
import Polysemy.Hasql.Test.Database (withTestStoreGen)
import Polysemy.Hasql.Test.Run (integrationTest)

newtype Id =
  Id { unId :: Int }
  deriving (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

data Rec =
  Rec {
    a :: Int,
    b :: Text
  }
  deriving (Eq, Show, Generic)

prog ::
  Member (Store (IdQuery Id) (Uid Id Rec)) r =>
  Uid Id Rec ->
  Sem r (Either DbError (Maybe (Uid Id Rec)))
prog specimen = do
  runError do
    Store.upsert specimen
    Store.fetch (IdQuery 2)

struct :: Data.Column
struct =
  tableStructure @(UidRep PrimaryKey Auto) @(Uid Id Rec)

table :: QueryTable (IdQuery Id) (Uid Id Rec)
table =
  queryTable

type RecType =
  'Kind.Tree ('NamedField "Rec") '[ADT (ADTMeta' (Product (UidRep PrimaryKey Auto)) (Uid Id Rec)) (Product (UidRep PrimaryKey Auto))] ('Kind.Prod (Uid Id Rec) '[
    'Kind.Tree ('NamedField "id") '[Newtype Id Int, PrimaryKey, Prim] ('Kind.Prim Id),
    'Kind.Tree ('NamedField "payload") '[ADT (ADTMeta' (Flatten Auto) Rec) (Flatten Auto)] (
      'Kind.Prod Rec '[
        'Kind.Tree ('NamedField "a") '[Prim] ('Kind.Prim Int),
        'Kind.Tree ('NamedField "b") '[Prim] ('Kind.Prim Text)
      ]
    )
  ])

testDerivation ::
  Tree.Tree DbParams ('TreeMeta ('NamedField "Rec") (Product (UidRep PrimaryKey Auto)) (Uid Id Rec)) RecType =>
  ()
testDerivation =
  ()

targetStructure :: Data.Column
targetStructure =
  Data.Column "rec" [text|"rec"|] "uid" def $ Data.Prod [
    Data.Column "id" [text|"id"|] "bigint" def { primaryKey = True } Data.Prim,
    Data.Column "a" [text|"a"|] "bigint" def Data.Prim,
    Data.Column "b" [text|"b"|] "text" def Data.Prim
  ]

test_pk :: UnitTest
test_pk =
  integrationTest do
    _ <- pure testDerivation
    targetStructure === struct
    result <- withTestStoreGen @Auto @(UidRep PrimaryKey Auto) @(IdQuery Id) @(Uid Id Rec) (restop @DbError (prog specimen))
    assertJust specimen =<< evalEither result
  where
    specimen =
      Uid 2 (Rec 5 "foo")
