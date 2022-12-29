{-# options_ghc -Wno-partial-type-signatures #-}

module Polysemy.Hasql.Test.MigrationTest where

import Path (reldir)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assertJust, runTestAuto)
import Prelude hiding (sum)
import Sqel.Data.Dd (Dd, DdK (DdK), type (:>) ((:>)))
import Sqel.Data.Migration (Migrations)
import Sqel.Data.Uid (Uid)
import Sqel.Migration.Consistency (migrationConsistency)
import Sqel.Migration.Table (migrateAuto)
import Sqel.Names (typeAs)
import Sqel.Prim (migrateDef, migrateDelete, migrateRename, prim, primNullable)
import Sqel.Product2 (prod, uidAs)

import Polysemy.Hasql.Effect.Database (Database)
import Polysemy.Hasql.Migration (migrateSem)
import Polysemy.Hasql.Test.Migration (testMigration)

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
    migrateDelete (migrateDef 0 prim) :>
    typeAs @"Pord" (prod (
      migrateDef 53 prim
    ))
  )

t2 :: Dd ('DdK _ _ (Uid Int64 Dat2) _)
t2 =
  uidAs @"dat" prim (
    migrateDef 15 prim :>
    typeAs @"Pord" (prod (
      prim :>
      primNullable
    ))
  )

tcur :: Dd ('DdK _ _ (Uid Int64 Dat) _)
tcur =
  uidAs @"dat" prim (
    migrateDef ("vunqach" :: Text) prim :>
    migrateRename @"number" prim :>
    prod (
      prim :>
      primNullable
    )
  )

migrations ::
  Members [Database !! DbError, Stop DbError] r =>
  Migrations (Sem r) [Uid Int64 Dat2, Uid Int64 Dat1, Uid Int64 Dat0] (Uid Int64 Dat)
migrations =
  migrateSem (
    migrateAuto t2 tcur :>
    migrateAuto t1 t2 :>
    migrateAuto t0 t1
  )

migrationErrors :: NonEmpty Text
migrationErrors =
  [
    "The migration table 'dat' has mismatched columns:",
    " • The column 'number' with type 'bigint' was removed.",
    "The composite type 'ph_type__pord' has mismatched columns:",
    " • The type of the column 'p1' was changed from 'text' to 'bigint'.",
    "The type 'ph_type__point' was removed."
  ]

test_migrationErrors :: UnitTest
test_migrationErrors =
  runTestAuto do
    fixtures <- Test.fixturePath [reldir|migration-error|]
    assertJust migrationErrors =<< migrationConsistency fixtures (migrations @[Database !! DbError, Stop DbError]) False

test_migrationConsistency :: UnitTest
test_migrationConsistency =
  runTestAuto do
    testMigration migrations False
