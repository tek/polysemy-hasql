module Polysemy.Hasql.Test.Migration where

import qualified Data.Text as Text
import Hedgehog.Internal.Property (failWith)
import Path (reldir)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Test as Test
import Polysemy.Test (Hedgehog, Test, liftH)
import Sqel.Data.Migration (Migrations)
import Sqel.Migration.Consistency (migrationConsistency)

import Polysemy.Hasql.Effect.Database (Database)

testMigration' ::
  Members [Test, Hedgehog IO, Embed IO] r =>
  Migrations r' old cur ->
  Bool ->
  Sem r ()
testMigration' migs write =
  withFrozenCallStack do
    dir <- Test.fixturePath [reldir|migration|]
    migrationConsistency dir migs write >>= \case
      Just errors ->
        liftH (failWith Nothing (toString (Text.intercalate "\n" (toList errors))))
      Nothing -> unit

testMigration ::
  Members [Test, Hedgehog IO, Embed IO] r =>
  Migrations (Sem (Database !! DbError : Stop DbError : r)) old cur ->
  Bool ->
  Sem r ()
testMigration write =
  withFrozenCallStack do
    testMigration' write
