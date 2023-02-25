module Polysemy.Hasql.Test.Run where

import Conc (interpretMaskFinal, interpretRace)
import Exon (exon)
import Hasql.Session (QueryError)
import Hedgehog (TestT)
import Hedgehog.Internal.Property (Failure)
import Log (Severity (Error), interpretLogStdoutLevelConc)
import Polysemy.Db.Data.DbConfig (DbConfig (DbConfig))
import Polysemy.Db.Data.DbConnectionError (DbConnectionError)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Db.Random (Random, runRandomIO)
import qualified Polysemy.Test as Hedgehog
import Polysemy.Test (Hedgehog, Test, runTestAuto)
import Polysemy.Test.Data.TestError (TestError)
import Polysemy.Time (GhcTime, interpretTimeGhc)
import System.Environment (lookupEnv)

import Polysemy.Hasql.Test.Database (TestConnectionEffects, withTestConnection)

data Err1 = Err1
data Err2 = Err2

data E1 :: Effect where
  E1 :: E1 m ()

makeSem ''E1

data E2 :: Effect where
  E2 :: E2 m ()

makeSem ''E2

f :: Members [E1 !! Err1, E2 !! Err2] r => Sem r ()
f =
  resume_ e1

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

dbConfig ::
  MonadIO m =>
  String ->
  Text ->
  m (Maybe DbConfig)
dbConfig envPrefix name = do
  traverse cons =<< (liftIO (lookupEnv [exon|#{envPrefix}_test_host|]))
  where
    cons host = do
      port <- parsePort =<< (fromMaybe "4321" <$> liftIO (lookupEnv [exon|#{envPrefix}_test_port|]))
      pure (DbConfig (fromString host) port (fromText name) (fromText name) (fromText name))
    parsePort p =
      case readMaybe p of
        Just a -> pure a
        Nothing -> error [exon|invalid port in env var $#{envPrefix}_test_port: #{p}|]

runIntegrationTestWith ::
  Members [Error Text, Embed IO] r =>
  HasCallStack =>
  String ->
  Text ->
  (DbConfig -> Sem (DbErrors ++ r) ()) ->
  Sem r ()
runIntegrationTestWith envPrefix name run =
  withFrozenCallStack do
    dbConfig envPrefix name >>= \case
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
  String ->
  Text ->
  (DbConfig -> Sem TestEffects ()) ->
  TestT IO ()
integrationTestWith envPrefix name run =
  withFrozenCallStack $ runTestAuto do
    r <- asyncToIOFinal $
      interpretRace $
      interpretMaskFinal $
      runError @Text $
      interpretLogStdoutLevelConc (Just Error) $
      runRandomIO $
      interpretTimeGhc $
      runIntegrationTestWith envPrefix name run
    Hedgehog.evalEither r

integrationTest ::
  HasCallStack =>
  String ->
  Text ->
  Sem (TestConnectionEffects ++ TestEffects) () ->
  TestT IO ()
integrationTest envPrefix name thunk =
  withFrozenCallStack do
    integrationTestWith envPrefix name \ conf -> withTestConnection conf thunk
