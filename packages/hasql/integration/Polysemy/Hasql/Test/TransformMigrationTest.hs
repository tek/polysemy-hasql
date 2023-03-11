{-# options_ghc -Wno-partial-type-signatures #-}

module Polysemy.Hasql.Test.TransformMigrationTest where

import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Test (UnitTest, assertEq)
import Sqel.Column (pk)
import Sqel.Data.Dd (Sqel, (:>) ((:>)))
import Sqel.Data.Migration (Mig (Mig), Migrations, migrate)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Data.Uid (Uid (Uid))
import Sqel.Migration.Transform (MigrateTransform, migrateTransform)
import Sqel.Names (typeAs)
import Sqel.PgType (tableSchema)
import Sqel.Prim (enum, prim, primAs, prims)
import Sqel.Product (prod)
import Sqel.Query (checkQuery)
import Sqel.Uid (uid)
import Zeugma (resumeTest)

import Polysemy.Hasql.Data.MigrateSem (MigrateSem)
import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Interpreter.DbTable (interpretTableMigrations, interpretTables)
import Polysemy.Hasql.Interpreter.Store (interpretStoreDb)
import Polysemy.Hasql.Test.RunIntegration (integrationTest)

data DatOld =
  DatOld {
    number :: Int,
    user :: Text,
    admin :: Bool
  }
  deriving stock (Eq, Show, Generic)

data Admin =
  Admin
  |
  NotAdmin
  deriving stock (Eq, Show, Generic)

adminBool :: Bool -> Admin
adminBool = \case
  True -> Admin
  False -> NotAdmin

data Meta =
  Meta {
    admin :: Admin,
    loggedIn :: Bool
  }
  deriving stock (Eq, Show, Generic)

data User =
  User {
    name :: Text,
    meta :: Meta
  }
  deriving stock (Eq, Show, Generic)

ddDatOld :: Sqel (Uid Int64 DatOld) _
ddDatOld =
  uid (pk prim) (typeAs @"User" (prod prims))

schemaOld :: TableSchema (Uid Int64 DatOld)
schemaOld =
  tableSchema ddDatOld

ddUser :: Sqel (Uid Int64 User) _
ddUser =
  uid (pk prim) (prod (prim :> prod (enum :> prim)))

schemaCur :: TableSchema (Uid Int64 User)
schemaCur =
  tableSchema ddUser

toUser :: DatOld -> User
toUser DatOld {..} =
  User {
    name = user,
    meta = Meta {admin = adminBool admin, loggedIn = False}
  }

migrations ::
  Migrations (MigrateSem r) ('[ 'Mig (Uid Int64 DatOld) (Uid Int64 User) (MigrateSem r) (MigrateTransform (MigrateSem r) (Uid Int64 DatOld) (Uid Int64 User))])
migrations =
  migrate (
    migrateTransform ddDatOld ddUser (pure . fmap (fmap toUser))
  )

target :: [Uid Int64 User]
target =
  [Uid 1 (User "user1" (Meta NotAdmin False)), Uid 2 (User "user2" (Meta Admin False))]

test_transformMigration :: UnitTest
test_transformMigration =
  integrationTest do
    interpretTables schemaOld $ interpretStoreDb schemaOld (checkQuery (primAs @"id") ddDatOld) $ resumeTest @DbError do
      Store.insert (Uid 1 (DatOld 1 "user1" False))
      Store.insert (Uid 2 (DatOld 2 "user2" True))
    resumeTest @DbError Database.release
    resumeTest @DbError Database.resetInit
    interpretTableMigrations schemaCur migrations $
      interpretStoreDb schemaCur (checkQuery (primAs @"id") ddUser) $
      restop @DbError @(Store _ _) do
        assertEq target =<< Store.fetchAll
