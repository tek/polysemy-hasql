module Polysemy.Hasql.Test.RunIntegration where

import Conc (interpretMaskFinal, interpretRace)
import Hasql.Session (QueryError)
import Hedgehog (TestT)
import Hedgehog.Internal.Property (Failure)
import Log (Severity (Error), interpretLogStdoutLevelConc)
import Polysemy.Db.Data.DbConfig (DbConfig)
import Polysemy.Db.Data.DbConnectionError (DbConnectionError)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Db.Random (Random, runRandomIO)
import qualified Polysemy.Test as Hedgehog
import Polysemy.Test (Hedgehog, Test, runTestAuto)
import Polysemy.Test.Data.TestError (TestError)
import Polysemy.Time (GhcTime, interpretTimeGhc)

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
    Random,
    Log,
    Error Text,
    Mask,
    Race,
    Async,
    Test,
    Fail,
    Error TestError,
    Hedgehog IO,
    Error Failure,
    Embed IO,
    Resource,
    Final IO
  ]

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

integrationTestWith ::
  HasCallStack =>
  (DbConfig -> Sem TestEffects ()) ->
  TestT IO ()
integrationTestWith run =
  withFrozenCallStack $ runTestAuto do
    r <- asyncToIOFinal $
      interpretRace $
      interpretMaskFinal $
      runError @Text $
      interpretLogStdoutLevelConc (Just Error) $
      runRandomIO $
      interpretTimeGhc $
      runIntegrationTestWith run
    Hedgehog.evalEither r

integrationTest ::
  HasCallStack =>
  Sem (TestConnectionEffects ++ TestEffects) () ->
  TestT IO ()
integrationTest thunk =
  withFrozenCallStack do
    integrationTestWith \ conf -> withTestConnection conf thunk
