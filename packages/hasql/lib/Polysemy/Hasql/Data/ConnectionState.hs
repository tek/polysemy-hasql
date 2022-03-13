module Polysemy.Hasql.Data.ConnectionState where

import Control.Concurrent (ThreadId)
import Control.Lens (makeClassy)
import Hasql.Connection (Connection)

data ConnectionState =
  ConnectionState {
    _count :: Int,
    _connection :: Maybe Connection,
    _activeCommand :: Maybe ThreadId
  }

makeClassy ''ConnectionState
