{-# options_ghc -Wno-partial-type-signatures #-}

module Polysemy.Hasql.Test.MigrationTest where

import Path (reldir)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assertJust, runTestAuto)
import Prelude hiding (sum)
import Sqel (
  Dd,
  Uid,
  migrate,
  migrateAuto,
  migrateDef,
  migrateDelete,
  migrateRename,
  prim,
  primNullable,
  prod,
  typeAs,
  uidAs,
  type (:>) ((:>)),
  )
import Sqel.Ext (AutoMigrations, DdK (DdK))
import Sqel.Migration.Consistency (migrationConsistency)

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
  uidAs @"dat" prim (prod (migrateDelete prim))

t1 :: Dd ('DdK _ _ (Uid Int64 Dat1) _)
t1 =
  uidAs @"dat" prim (prod (
    migrateDelete (migrateDef 0 prim) :>
    typeAs @"Pord" (prod (
      migrateDef 53 prim
    ))
  ))

t2 :: Dd ('DdK _ _ (Uid Int64 Dat2) _)
t2 =
  uidAs @"dat" prim (prod (
    migrateDef 15 prim :>
    typeAs @"Pord" (prod (
      prim :>
      primNullable
    ))
  ))

tcur :: Dd ('DdK _ _ (Uid Int64 Dat) _)
tcur =
  uidAs @"dat" prim (prod (
    migrateDef ("vunqach" :: Text) prim :>
    migrateRename @"number" prim :>
    prod (
      prim :>
      primNullable
    )
  ))

migrations ::
  AutoMigrations (Sem r) [Uid Int64 Dat2, Uid Int64 Dat1, Uid Int64 Dat0] (Uid Int64 Dat)
migrations =
  migrate (
    migrateAuto t2 tcur :>
    migrateAuto t1 t2 :>
    migrateAuto t0 t1
  )

migrationErrors :: NonEmpty Text
migrationErrors =
  [
    "The migration table 'dat' has mismatched columns:",
    " • The column 'number' with type 'bigint' was removed.",
    "The composite type 'sqel_type__pord' has mismatched columns:",
    " • The type of the column 'p1' was changed from 'text' to 'bigint'.",
    "The type 'sqel_type__point' was removed."
  ]

test_migrationErrors :: UnitTest
test_migrationErrors =
  runTestAuto do
    fixtures <- Test.fixturePath [reldir|migration-error|]
    assertJust migrationErrors =<< migrationConsistency fixtures migrations False

-- TODO error message claims type change if a column constraint differs
test_migrationConsistency :: UnitTest
test_migrationConsistency =
  runTestAuto do
    testMigration migrations False
