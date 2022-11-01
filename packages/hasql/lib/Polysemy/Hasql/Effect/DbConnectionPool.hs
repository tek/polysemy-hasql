module Polysemy.Hasql.Effect.DbConnectionPool where

import Hasql.Connection (Connection)

import Polysemy.Hasql.Data.ConnectionTag (ConnectionTag)

data DbConnectionPool :: Effect where
  Acquire :: ConnectionTag -> DbConnectionPool m Connection
  Free :: ConnectionTag -> DbConnectionPool m ()
  Release :: ConnectionTag -> DbConnectionPool m ()
  Use :: ConnectionTag -> m a -> DbConnectionPool m a
  Kill :: ConnectionTag -> DbConnectionPool m ()
  UnsafeGet :: ConnectionTag -> DbConnectionPool m (Maybe Connection)

makeSem ''DbConnectionPool
