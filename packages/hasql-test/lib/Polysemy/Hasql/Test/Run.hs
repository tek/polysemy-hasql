module Polysemy.Hasql.Test.Run where

import Data.UUID (UUID)
import Exon (exon)
import Hasql.Session (QueryError)
import Log (Severity (Error))
import Polysemy.Db (DbName, DbPassword, DbUser, interpretRandom)
import Polysemy.Db.Data.DbConfig (DbConfig (DbConfig))
import Polysemy.Db.Data.DbConnectionError (DbConnectionError)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Db.Effect.Random (Random)
import Polysemy.Test (UnitTest)
import System.Environment (lookupEnv)
import Time (GhcTime, interpretTimeGhc)
import Zeugma (TestError (TestError), TestStack, runTest, runTestLevel, stopTest)

import Polysemy.Hasql.Test.Database (TestConnectionEffects, withTestConnection)

data EnvDb =
  EnvDb {
    envPrefix :: String,
    dbName :: DbName,
    dbUser :: DbUser,
    dbPassword :: DbPassword
  }
  deriving stock (Eq, Show, Generic)

envDb :: String -> EnvDb
envDb name =
  EnvDb {envPrefix = name, dbName = fromString name, dbUser = fromString name, dbPassword = fromString name}

instance IsString EnvDb where
  fromString = envDb

type DbErrors =
  [
    Stop DbConnectionError,
    Stop DbError,
    Stop QueryError,
    Error InitDbError,
    Error DbError
  ]

type TestEffects =
  DbErrors ++ [GhcTime, Random UUID] ++ TestStack

envDbConfig ::
  MonadIO m =>
  EnvDb ->
  m (Maybe DbConfig)
envDbConfig EnvDb {..} = do
  traverse cons =<< (liftIO (lookupEnv [exon|#{envPrefix}_test_host|]))
  where
    cons host = do
      port <- parsePort =<< (fromMaybe "4321" <$> liftIO (lookupEnv [exon|#{envPrefix}_test_port|]))
      pure (DbConfig (fromString host) port dbName dbUser dbPassword)
    parsePort p =
      case readMaybe p of
        Just a -> pure a
        Nothing -> error [exon|invalid port in env var $#{envPrefix}_test_port: #{p}|]

interpretDbErrors ::
  Members [Error TestError, Embed IO] r =>
  HasCallStack =>
  InterpretersFor DbErrors r
interpretDbErrors =
  withFrozenCallStack $
  mapError @DbError (TestError . show) .
  mapError @InitDbError (TestError . show) .
  stopTest @QueryError .
  stopTest @DbError .
  stopTest @DbConnectionError

runIntegrationTestConfig ::
  Members [Error TestError, Embed IO] r =>
  HasCallStack =>
  DbConfig ->
  (DbConfig -> Sem (DbErrors ++ r) ()) ->
  Sem r ()
runIntegrationTestConfig conf run =
  withFrozenCallStack do
    interpretDbErrors (run conf)

runIntegrationTestEnv ::
  Members [Error TestError, Embed IO] r =>
  HasCallStack =>
  EnvDb ->
  (DbConfig -> Sem (DbErrors ++ r) ()) ->
  Sem r ()
runIntegrationTestEnv econf run =
  withFrozenCallStack do
    envDbConfig econf >>= \case
      Just conf ->
        interpretDbErrors (run conf)
      Nothing ->
        unit

integrationTestConfig ::
  HasCallStack =>
  DbConfig ->
  Sem TestEffects () ->
  UnitTest
integrationTestConfig conf run =
  withFrozenCallStack $
  runTest $
  interpretRandom $
  interpretTimeGhc $
  runIntegrationTestConfig conf (const run)

integrationTestLevelWith ::
  HasCallStack =>
  Severity ->
  EnvDb ->
  (DbConfig -> Sem TestEffects ()) ->
  UnitTest
integrationTestLevelWith level econf run =
  withFrozenCallStack $
  runTestLevel level $
  interpretRandom $
  interpretTimeGhc $
  runIntegrationTestEnv econf run

integrationTestWith ::
  HasCallStack =>
  EnvDb ->
  (DbConfig -> Sem TestEffects ()) ->
  UnitTest
integrationTestWith =
  withFrozenCallStack .
  integrationTestLevelWith Error

integrationTestLevel ::
  HasCallStack =>
  Severity ->
  EnvDb ->
  Sem (TestConnectionEffects ++ TestEffects) () ->
  UnitTest
integrationTestLevel level econf test =
  withFrozenCallStack do
    integrationTestLevelWith level econf \ conf -> withTestConnection conf test

integrationTest ::
  HasCallStack =>
  EnvDb ->
  Sem (TestConnectionEffects ++ TestEffects) () ->
  UnitTest
integrationTest econf test =
  withFrozenCallStack do
    integrationTestLevel Error econf test
