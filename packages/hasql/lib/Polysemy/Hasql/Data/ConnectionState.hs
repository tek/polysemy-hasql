module Polysemy.Hasql.Data.ConnectionState where

import Control.Concurrent (ThreadId)
import Hasql.Connection (Connection)

import Polysemy.Hasql.Data.InitDb (ClientTag)

data ConnectionState =
  ConnectionState {
    count :: Int,
    connection :: Maybe Connection,
    activeCommands :: Map ThreadId Int
  }
  deriving stock (Generic)

instance Default ConnectionState where
  def =
    ConnectionState 0 Nothing mempty

data ConnectionsState =
  ConnectionsState {
    counter :: Integer,
    clientInits :: Map ClientTag Int
  }
  deriving stock (Eq, Show, Generic)
