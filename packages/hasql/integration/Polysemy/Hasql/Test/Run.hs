module Polysemy.Hasql.Test.Run where

import Conc (interpretMaskFinal, interpretRace)
import Hasql.Session (QueryError)
import Hedgehog (TestT)
import Hedgehog.Internal.Property (Failure)
import Log (Severity (Trace), interpretLogStdoutLevelConc)
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

type TestEffects =
  [
    GhcTime,
    Random,
    Log,
    Stop DbConnectionError,
    Stop DbError,
    Stop QueryError,
    Stop Text,
    Error InitDbError,
    Error DbError,
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

integrationTestWith ::
  HasCallStack =>
  (DbConfig -> Sem TestEffects ()) ->
  TestT IO ()
integrationTestWith run =
  withFrozenCallStack do
    dbConfig >>= \case
      Just conf -> do
        runTestAuto do
          r <-
            asyncToIOFinal $
            interpretRace $
            interpretMaskFinal $
            runError @Text $
            mapError @DbError @Text show $
            mapError @InitDbError @Text show $
            stopToError @Text $
            mapStop @QueryError @Text show $
            mapStop @DbError @Text show $
            mapStop @DbConnectionError @Text show $
            interpretLogStdoutLevelConc (Just Trace) $
            runRandomIO $
            interpretTimeGhc $
            run conf
          Hedgehog.evalEither r
      Nothing ->
        unit

integrationTest ::
  HasCallStack =>
  Sem (TestConnectionEffects ++ TestEffects) () ->
  TestT IO ()
integrationTest thunk =
  withFrozenCallStack do
    integrationTestWith \ conf -> withTestConnection conf thunk

integrationTestWithDb ::
  HasCallStack =>
  (DbConfig -> Sem (TestConnectionEffects ++ TestEffects) ()) ->
  TestT IO ()
integrationTestWithDb thunk =
  withFrozenCallStack do
    integrationTestWith \ conf -> withTestConnection conf (thunk conf)
