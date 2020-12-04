module Polysemy.Hasql.Data.DbConnection where

data DbConnection c :: Effect where
  ConnectWithInit :: (c -> m ()) -> DbConnection c m c
  Disconnect :: DbConnection c m ()
  Reset :: DbConnection c m ()

makeSem ''DbConnection

connect ::
  Member (DbConnection c) r =>
  Sem r c
connect =
  connectWithInit (const unit)
