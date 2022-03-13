module Polysemy.Hasql.Test.DbConfig where

import Exon (exon)
import Polysemy.Db.Data.DbConfig (DbConfig (DbConfig))
import System.Environment (lookupEnv)

dbConfig ::
  MonadIO m =>
  m (Maybe DbConfig)
dbConfig = do
  traverse cons =<< (liftIO (lookupEnv "polysemy_db_test_host"))
  where
    cons host = do
      port <- parsePort =<< (fromMaybe "4321" <$> liftIO (lookupEnv "polysemy_db_test_port"))
      pure (DbConfig (fromString host) port "polysemy-db" "polysemy-db" "polysemy-db")
    parsePort p =
      case readMaybe p of
        Just a -> pure a
        Nothing -> error [exon|invalid port in env var $polysemy_db_test_port: #{p}|]
