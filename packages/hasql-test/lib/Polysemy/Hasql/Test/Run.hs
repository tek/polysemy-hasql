module Polysemy.Hasql.Test.Run where

import Conc (interpretAtomic, timeout_)
import Data.UUID (UUID)
import Exon (exon)
import Hasql.Session (QueryError)
import qualified Log
import Log (Severity (Error))
import Polysemy.Db (DbName, DbPassword, DbUser, interpretRandom)
import Polysemy.Db.Data.DbConfig (DbConfig (DbConfig))
import Polysemy.Db.Data.DbConnectionError (DbConnectionError)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Db.Effect.Random (Random)
import Polysemy.Test (UnitTest)
import System.Environment (lookupEnv)
import qualified Time as Time
import Time (GhcTime, MilliSeconds (MilliSeconds), Seconds (Seconds), interpretTimeGhc)
import Zeugma (TestStack, runTest, runTestLevel)

import qualified Polysemy.Hasql.Effect.DbConnectionPool as DbConnectionPool
import Polysemy.Hasql.Effect.DbConnectionPool (DbConnectionPool)
import Polysemy.Hasql.Interpreter.DbConnectionPool (interpretDbConnectionPool)
import Polysemy.Hasql.Test.Database (TestConnectionEffects, withTestConnection)

data EnvDb =
  EnvDb {
    envPrefix :: String,
    dbName :: DbName,
    dbUser :: DbUser,
    dbPassword :: DbPassword,
    fatal :: Bool,
    notify :: Bool,
    wait :: Maybe Seconds
  }
  deriving stock (Eq, Show, Generic)

envDb :: String -> EnvDb
envDb name =
  EnvDb {
    envPrefix = name,
    dbName = fromString name,
    dbUser = fromString name,
    dbPassword = fromString name,
    fatal = False,
    notify = True,
    wait = Just 30
  }

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
  Members [Error Text, Embed IO] r =>
  EnvDb ->
  Sem r (Either Text DbConfig)
envDbConfig EnvDb {..} = do
  cons =<< embed (lookupEnv hostVar)
  where
    cons = \case
      Just host -> do
        port <- parsePort =<< (fromMaybe "5432" <$> embed (lookupEnv portVar))
        pure (Right (DbConfig (fromString host) port dbName dbUser dbPassword))
      Nothing ->
        pure (Left (toText hostVar))
    parsePort p =
      case readMaybe p of
        Just a -> pure a
        Nothing -> throw (fromString [exon|Invalid port in env var '$#{portVar}': #{p}|])
    hostVar = [exon|#{envPrefix}_test_host|]
    portVar = [exon|#{envPrefix}_test_port|]

interpretDbErrors ::
  Members [Error Text, Embed IO] r =>
  HasCallStack =>
  InterpretersFor DbErrors r
interpretDbErrors =
  withFrozenCallStack $
  mapError @DbError show .
  mapError @InitDbError show .
  stopToErrorWith @QueryError show .
  stopToErrorWith @DbError show .
  stopToErrorWith @DbConnectionError show

serverInoperative ::
  Members [AtomicState (Maybe DbConnectionError), Error Text] r =>
  Seconds ->
  Sem r ()
serverInoperative (Seconds timeout) = do
  err <- maybe noResponse show <$> atomicGet
  throw (fromString [exon|Server didn't start up within #{show timeout} seconds:
  #{err}
|])
  where
    noResponse = "Connection attempt did not terminate."

waitForServer ::
  Members [DbConnectionPool !! DbConnectionError, Error Text, Time t d, Race, Embed IO] r =>
  Seconds ->
  Sem r ()
waitForServer timeout = do
  interpretAtomic Nothing do
    timeout_ (serverInoperative timeout) timeout do
      Time.while (MilliSeconds 50) do
        (False <$ DbConnectionPool.acquire "boot") !! \ e -> True <$ atomicPut (Just e)

runIntegrationAppConfig ::
  Members [Error Text, Time t d, Log, Resource, Race, Embed IO, Final IO] r =>
  HasCallStack =>
  Maybe Seconds ->
  DbConfig ->
  (DbConfig -> Sem (DbErrors ++ r) ()) ->
  Sem r ()
runIntegrationAppConfig waitTimeout conf run =
  withFrozenCallStack do
    for_ waitTimeout \ timeout ->
      interpretDbConnectionPool conf Nothing Nothing do
        waitForServer timeout
    interpretDbErrors (run conf)

runIntegrationAppEnv ::
  Members [Error Text, Time t d, Log, Resource, Race, Embed IO, Final IO] r =>
  HasCallStack =>
  EnvDb ->
  (DbConfig -> Sem (DbErrors ++ r) ()) ->
  Sem r ()
runIntegrationAppEnv econf run =
  withFrozenCallStack do
    envDbConfig econf >>= \case
      Right conf ->
        runIntegrationAppConfig econf.wait conf run
      Left var -> do
        let msg = [exon|Can't run integration test, env var unset: #{var}|]
        when econf.fatal (throw msg)
        when econf.notify (Log.crit msg)

integrationApp ::
  Members [Error Text, Log, Resource, Race, Embed IO, Final IO] r =>
  HasCallStack =>
  EnvDb ->
  (DbConfig -> Sem (DbErrors ++ [GhcTime, Random UUID] ++ r) ()) ->
  Sem r ()
integrationApp econf run =
  withFrozenCallStack $
  interpretRandom $
  interpretTimeGhc $
  runIntegrationAppEnv econf run

integrationTestConfig ::
  HasCallStack =>
  Maybe Seconds ->
  DbConfig ->
  Sem TestEffects () ->
  UnitTest
integrationTestConfig waitTimeout conf run =
  withFrozenCallStack $
  runTest $
  interpretRandom $
  interpretTimeGhc $
  runIntegrationAppConfig waitTimeout conf (const run)

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
  runIntegrationAppEnv econf run

integrationTestWith ::
  HasCallStack =>
  EnvDb ->
  (DbConfig -> Sem TestEffects ()) ->
  UnitTest
integrationTestWith =
  withFrozenCallStack do
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
