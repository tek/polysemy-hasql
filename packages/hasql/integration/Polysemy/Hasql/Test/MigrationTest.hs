{-# options_ghc -Wno-partial-type-signatures #-}

module Polysemy.Hasql.Test.MigrationTest where

import Lens.Micro.Extras (view)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Query as Query
import Polysemy.Db.Effect.Query (Query)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Test (UnitTest, assertEq)
import Prelude hiding (sum)
import Sqel.Data.Dd (Dd, DdK (DdK), type (:>) ((:>)))
import Sqel.Data.Migration (AutoMigrations, migrate)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Data.Uid (Uid (Uid))
import Sqel.Migration.Table (migrateAuto)
import Sqel.PgType (tableSchema)
import Sqel.Prim (array, migrateDef, migrateDelete, migrateRename, migrateRenameType, prim, primAs, primNullable)
import Sqel.Product (Product (prod))
import Sqel.Query (checkQuery)
import Sqel.Uid (uidAs)

import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Interpreter.DbTable (interpretTableMigrations, interpretTables)
import Polysemy.Hasql.Interpreter.Query (interpretQueryDd)
import Polysemy.Hasql.Interpreter.Store (interpretStoreDb)
import Polysemy.Hasql.Migration (MigrateSem)
import Polysemy.Hasql.Test.RunIntegration (integrationTest)

data PordOld =
  PordOld {
    p1 :: Int64
  }
  deriving stock (Eq, Show, Generic)

data Pord =
  Pord {
    p1 :: Int64,
    p2 :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

data Dat0 =
  Dat0 {
    old :: Text,
    names :: [Text]
  }
  deriving stock (Eq, Show, Generic)

data Dat1 =
  Dat1 {
    size :: Int64,
    names :: [Text],
    pordOld :: PordOld
  }
  deriving stock (Eq, Show, Generic)

data Dat2 =
  Dat2 {
    number :: Int64,
    names :: [Text],
    pord :: Pord
  }
  deriving stock (Eq, Show, Generic)

data Dat =
  Dat {
    name :: Text,
    names :: [Text],
    num :: Int64,
    pord :: Pord
  }
  deriving stock (Eq, Show, Generic)

data Q =
  Q {
    name :: Text
  }
  deriving stock (Eq, Show, Generic)

t0 :: Dd ('DdK _ _ (Uid Int64 Dat0) _)
t0 =
  uidAs @"dat" prim (prod (migrateDelete prim :> array prim))

t1 :: Dd ('DdK _ _ (Uid Int64 Dat1) _)
t1 =
  uidAs @"dat" prim (prod (
    migrateDelete (migrateDef 0 prim) :>
    array prim :>
    prod (
      migrateDef 53 prim
    )
  ))

t2 :: Dd ('DdK _ _ (Uid Int64 Dat2) _)
t2 =
  uidAs @"dat" prim (prod (
    migrateDef 15 prim :>
    array prim :>
    migrateRename @"pordOld" (migrateRenameType @"sqel_type__PordOld" (prod (
      prim :>
      primNullable
    )))
  ))

tcur :: Dd ('DdK _ _ (Uid Int64 Dat) _)
tcur =
  uidAs @"dat" prim (prod (
    migrateDef ("vunqach" :: Text) prim :>
    array prim :>
    migrateRename @"number" prim :>
    prod (
      prim :>
      primNullable
    )
  ))

q :: Dd ('DdK _ _ Q _)
q =
  prod (primAs @"name")

schemaOld :: TableSchema (Uid Int64 Dat1)
schemaOld =
  tableSchema t1

schemaCur :: TableSchema (Uid Int64 Dat)
schemaCur =
  tableSchema tcur

migrations ::
  AutoMigrations (MigrateSem r) [Uid Int64 Dat2, Uid Int64 Dat1, Uid Int64 Dat0] (Uid Int64 Dat)
migrations =
  migrate (
    migrateAuto t2 tcur :>
    migrateAuto t1 t2 :>
    migrateAuto t0 t1
  )

test_migration :: UnitTest
test_migration =
  integrationTest do
    interpretTables schemaOld $ interpretStoreDb schemaOld (checkQuery (primAs @"id") t1) $ restop @DbError do
      Store.insert (Uid 3 (Dat1 11 ["0"] (PordOld 93)))
      Store.insert (Uid 4 (Dat1 55 ["0", "1"] (PordOld 78)))
    restop @DbError Database.release
    restop @DbError Database.resetInit
    interpretTableMigrations schemaCur migrations $
      interpretStoreDb schemaCur (checkQuery (primAs @"id") tcur) $
      interpretQueryDd @[_] tcur tcur q $
      restop @DbError @(Query _ _) $
      restop @DbError @(Store _ _) do
        Store.insert (Uid 1 (Dat "1" ["11"] 5 (Pord 10 (Just "new 1"))))
        Store.insert (Uid 2 (Dat "2" ["22", "33"] 5 (Pord 10 (Just "new 2"))))
        assertEq [3, 4] =<< fmap (view #id) <$> Query.query (Q "vunqach")
        assertEq [2] =<< fmap (view #id) <$> Query.query (Q "2")
