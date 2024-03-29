module Polysemy.Hasql.Test.Migration where

import qualified Data.Text as Text
import Hedgehog.Internal.Property (failWith)
import Path (reldir)
import qualified Polysemy.Test as Test
import Polysemy.Test (Hedgehog, Test, liftH)
import Sqel (Def)
import Sqel.Migration (TableDdl, migrationConsistency)

testMigration' ::
  ∀ table migs r .
  Members [Test, Hedgehog IO, Embed IO] r =>
  TableDdl Def (Sem r) table migs ->
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
  ∀ table migs r .
  Members [Test, Hedgehog IO, Embed IO] r =>
  TableDdl Def (Sem r) table migs ->
  Bool ->
  Sem r ()
testMigration migs write =
  withFrozenCallStack do
    testMigration' migs write
