module Polysemy.Hasql.Test.RunIntegration where

import Data.UUID (UUID)
import Hasql.Session (QueryError)
import Hedgehog (TestT)
import Log (Severity (Error))
import Polysemy.Db.Data.DbConfig (DbConfig)
import Polysemy.Db.Data.DbConnectionError (DbConnectionError)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Db.Effect.Random (Random)
import Polysemy.Db.Interpreter.Random (interpretRandom)
import Polysemy.Time (GhcTime, interpretTimeGhc)
import Zeugma.Run (TestStack, runTestLevel)

import Polysemy.Hasql.Test.Database (TestConnectionEffects, withTestConnection)
import Polysemy.Hasql.Test.DbConfig (dbConfig)

type DbErrors =
  [
    Stop DbConnectionError,
    Stop DbError,
    Stop QueryError,
    Stop Text,
    Error InitDbError,
    Error DbError
  ]

type TestEffects =
  DbErrors ++ [
    GhcTime,
    Random UUID
  ] ++ TestStack

runIntegrationTestWith ::
  Members [Error Text, Embed IO] r =>
  HasCallStack =>
  (DbConfig -> Sem (DbErrors ++ r) ()) ->
  Sem r ()
runIntegrationTestWith run =
  withFrozenCallStack do
    dbConfig >>= \case
      Just conf ->
        mapError @DbError @Text show $
        mapError @InitDbError @Text show $
        stopToError @Text $
        mapStop @QueryError @Text show $
        mapStop @DbError @Text show $
        mapStop @DbConnectionError @Text show $
        run conf
      Nothing ->
        unit

integrationTestLevelWith ::
  HasCallStack =>
  Severity ->
  (DbConfig -> Sem TestEffects ()) ->
  TestT IO ()
integrationTestLevelWith level run =
  withFrozenCallStack $ runTestLevel level $ interpretRandom $ interpretTimeGhc $ runIntegrationTestWith run

integrationTestWith ::
  HasCallStack =>
  (DbConfig -> Sem TestEffects ()) ->
  TestT IO ()
integrationTestWith =
  integrationTestLevelWith Error

integrationTestLevel ::
  HasCallStack =>
  Severity ->
  Sem (TestConnectionEffects ++ TestEffects) () ->
  TestT IO ()
integrationTestLevel level thunk =
  withFrozenCallStack do
    integrationTestLevelWith level \ conf -> withTestConnection conf thunk

integrationTest ::
  HasCallStack =>
  Sem (TestConnectionEffects ++ TestEffects) () ->
  TestT IO ()
integrationTest =
  integrationTestLevel Error
