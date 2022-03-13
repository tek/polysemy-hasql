module Polysemy.Hasql.Test.Run where

import GHC.Stack (withFrozenCallStack)
import Hasql.Session (QueryError)
import Hedgehog (TestT)
import Polysemy.Db.Data.DbConfig (DbConfig)
import Polysemy.Db.Data.DbConnectionError (DbConnectionError)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Db.Random (Random, runRandomIO)
import Polysemy.Log (interpretLogNull)
import qualified Polysemy.Test as Hedgehog
import Polysemy.Test (Hedgehog, Test, runTestAuto)
import Polysemy.Test.Data.TestError (TestError)
import Polysemy.Time (GhcTime, interpretTimeGhc)

import Polysemy.Hasql (HasqlConnection)
import Polysemy.Hasql.Data.Database (Database)
import Polysemy.Hasql.Test.Database (withTestConnection)
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
    Test,
    Fail,
    Error TestError,
    Hedgehog IO,
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
            runError @Text $
            mapError @DbError @Text show $
            mapError @InitDbError @Text show $
            stopToError @Text $
            mapStop @QueryError @Text show $
            mapStop @DbError @Text show $
            mapStop @DbConnectionError @Text show $
            interpretLogNull $
            runRandomIO $
            interpretTimeGhc $
            run conf
          Hedgehog.evalEither r
      Nothing ->
        unit

integrationTest ::
  HasCallStack =>
  Sem (Database !! DbError : HasqlConnection : TestEffects) () ->
  TestT IO ()
integrationTest thunk =
  withFrozenCallStack do
    integrationTestWith \ conf -> withTestConnection conf thunk

integrationTestWithDb ::
  HasCallStack =>
  (DbConfig -> Sem (Database !! DbError : HasqlConnection : TestEffects) ()) ->
  TestT IO ()
integrationTestWithDb thunk =
  withFrozenCallStack do
    integrationTestWith \ conf -> withTestConnection conf (thunk conf)
