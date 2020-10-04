module Polysemy.Hasql.Test.DbConfig where

import System.Environment (lookupEnv)

import Polysemy.Db.Data.DbConfig (DbConfig(DbConfig))

dbConfig ::
  MonadIO m =>
  m (Maybe DbConfig)
dbConfig = do
  traverse cons =<< (liftIO (lookupEnv "polysemy_db_test_host"))
  where
    cons host = do
      port <- parsePort =<< (fromMaybe "4321" <$> liftIO (lookupEnv "polysemy_db_test_port"))
      pure (DbConfig (fromString host) port "polysemy-db-test" "polysemy-db-test" "polysemy-db-test")
    parsePort p =
      case readMaybe p of
        Just a -> pure a
        Nothing -> error [qt|invalid port in env var $polysemy_db_test_port: #{p}|]
