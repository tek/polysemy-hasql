{-# options_ghc -Wno-partial-type-signatures #-}

module Polysemy.Hasql.Test.Dsl.MigrationTest where

import Hasql.Statement (Statement)
import Lens.Micro.Extras (view)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import qualified Polysemy.Db.Effect.StoreQuery as StoreQuery
import Polysemy.Db.Effect.StoreQuery (StoreQuery (Basic))
import Polysemy.Test (UnitTest, assertEq)
import Prelude hiding (sum)
import Sqel.Data.Dd (Dd, DdK (DdK), type (:>) ((:>)))
import Sqel.Data.Migration (Migrations)
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Data.Uid (Uid (Uid))
import Sqel.Migration.Table (migrateAuto)
import Sqel.Names (typeAs)
import Sqel.PgType (tableSchema)
import Sqel.Prim (migrateDelete, migrateRename, prim, primAs, primDef, primNullable)
import Sqel.Product (prod, uidAs)
import Sqel.Query (checkQuery)
import Sqel.Statement (qStatement)

import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database)
import Polysemy.Hasql.Interpreter.Store (interpretManagedTable, interpretStoreDb, interpretTableMigrations)
import Polysemy.Hasql.Migration (migrateSem)
import Polysemy.Hasql.Test.Run (integrationTest)

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
    old :: Text
  }
  deriving stock (Eq, Show, Generic)

data Dat1 =
  Dat1 {
    size :: Int64,
    pord :: PordOld
  }
  deriving stock (Eq, Show, Generic)

data Dat2 =
  Dat2 {
    number :: Int64,
    pord :: Pord
  }
  deriving stock (Eq, Show, Generic)

data Dat =
  Dat {
    name :: Text,
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
  uidAs @"dat" prim (migrateDelete prim)

t1 :: Dd ('DdK _ _ (Uid Int64 Dat1) _)
t1 =
  uidAs @"dat" prim (
    migrateDelete (primDef 0 prim) :>
    typeAs @"Pord" (prod (
      primDef 53 prim
    ))
  )

t2 :: Dd ('DdK _ _ (Uid Int64 Dat2) _)
t2 =
  uidAs @"dat" prim (
    primDef 15 prim :>
    typeAs @"Pord" (prod (
      prim :>
      primNullable
    ))
  )

tcur :: Dd ('DdK _ _ (Uid Int64 Dat) _)
tcur =
  uidAs @"dat" prim (
    primDef ("vunqach" :: Text) prim :>
    migrateRename @"number" prim :>
    prod (
      prim :>
      primNullable
    )
  )

q :: Dd ('DdK _ _ Q _)
q =
  prod (primAs @"name")

schemaOld :: TableSchema (Uid Int64 Dat1)
schemaOld =
  tableSchema t1

schemaCur :: TableSchema (Uid Int64 Dat)
schemaCur =
  tableSchema tcur

checkedQ :: QuerySchema Q (Uid Int64 Dat)
checkedQ =
  checkQuery q tcur

checkedQStm :: Statement Q [Uid Int64 Dat]
checkedQStm =
  qStatement checkedQ schemaCur

interpretQ ::
  Member (Database !! DbError) r =>
  InterpreterFor (StoreQuery Q [Uid Int64 Dat] !! DbError) r
interpretQ =
  interpretResumable \case
    Basic params ->
      restop (Database.statement params checkedQStm)

migrations ::
  Members [Database !! DbError, Stop DbError] r =>
  Migrations (Sem r) [Uid Int64 Dat2, Uid Int64 Dat1, Uid Int64 Dat0] (Uid Int64 Dat)
migrations =
  migrateSem (
    migrateAuto t2 tcur :>
    migrateAuto t1 t2 :>
    migrateAuto t0 t1
  )

test_dslMigration :: UnitTest
test_dslMigration =
  integrationTest do
    interpretManagedTable schemaOld $ interpretStoreDb schemaOld (checkQuery (primAs @"id") t1) $ restop @DbError do
      Store.insert (Uid 3 (Dat1 11 (PordOld 93)))
      Store.insert (Uid 4 (Dat1 55 (PordOld 78)))
    restop @DbError Database.release
    restop @DbError Database.resetInit
    interpretTableMigrations schemaCur migrations $
      interpretStoreDb schemaCur (checkQuery (primAs @"id") tcur) $
      interpretQ $
      restop @DbError @(StoreQuery _ _) $
      restop @DbError @(Store _ _) do
        Store.insert (Uid 1 (Dat "1" 5 (Pord 10 (Just "new 1"))))
        Store.insert (Uid 2 (Dat "2" 5 (Pord 10 (Just "new 2"))))
        assertEq [3, 4] =<< fmap (view #id) <$> StoreQuery.basic (Q "vunqach")
        assertEq [2] =<< fmap (view #id) <$> StoreQuery.basic (Q "2")
