module Polysemy.Hasql.Data.DbConnection where

-- |This effect's interface provides direct access to a connection (normally 'Hasql.Connection.Connection'), in the
-- shape of a higher-order callback effect that counts failed connection attempts.
--
-- For full constructor documentation, consult the module "Polysemy.Hasql.Data.DbConnection".
data DbConnection c :: Effect where
  Use :: (Int -> c -> m a) -> DbConnection c m a
  Disconnect :: DbConnection c m ()
  Reset :: DbConnection c m ()
  Info :: DbConnection c m (Text, Int)

makeSem ''DbConnection
