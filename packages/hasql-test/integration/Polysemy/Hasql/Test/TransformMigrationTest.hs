module Polysemy.Hasql.Test.TransformMigrationTest where

import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Test (UnitTest, assertEq)
import Prelude hiding (Enum)
import Sqel (Def, Enum, Gen, Prim, Prod, Sqel, Uid (Uid), UidTable, query_Int, sqel)
import qualified Sqel.Migration as Sqel
import Sqel.Migration (Migrate, (-->))
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

type Table_DatOld = UidTable "user" Int64 DatOld Prim Gen

table_DatOld :: Sqel Table_DatOld
table_DatOld = sqel

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

type Table_User = UidTable "user" Int64 User Prim (Prod [Prim, Prod [Enum, Prim]])

table_User :: Sqel Table_User
table_User = sqel

toUser :: DatOld -> User
toUser DatOld {..} =
  User {
    name = user,
    meta = Meta {admin = adminBool admin, loggedIn = False}
  }

migrations ::
  ∀ r .
  Member Log r =>
  Migrate Def (MigrateSem r) [Table_User, Table_DatOld]
migrations =
  table_DatOld --> Sqel.transform trans table_User
  where
    trans :: [Uid Int64 DatOld] -> MigrateSem r [Uid Int64 User]
    trans = pure @(MigrateSem r) . fmap (fmap toUser)

target :: [Uid Int64 User]
target =
  [Uid 1 (User "user1" (Meta NotAdmin False)), Uid 2 (User "user2" (Meta Admin False))]

test_transformMigration :: UnitTest
test_transformMigration =
  integrationTest do
    interpretTables table_DatOld $ interpretStoreDb query_Int table_DatOld $ resumeTest @DbError do
      Store.insert (Uid 1 (DatOld 1 "user1" False))
      Store.insert (Uid 2 (DatOld 2 "user2" True))
    resumeTest @DbError Database.release
    resumeTest @DbError Database.resetInit
    interpretTableMigrations migrations $
      interpretStoreDb query_Int table_User $
      restop @DbError @(Store _ _) do
        assertEq target =<< Store.fetchAll
