module Polysemy.Hasql.AtomicState where

import Polysemy.Db.AtomicState (interpretAtomicStateStore)
import Polysemy.Db.Data.InitDbError (InitDbError)

import Polysemy.Hasql.Crud (interpretCrudSingleton)
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import Polysemy.Hasql.Data.Query (Query)
import Polysemy.Hasql.Store (interpretStoreDb)

interpretAtomicStateDb ::
  Show e =>
  Members [Query () d, ManagedTable d !! e, Error InitDbError] r =>
  d ->
  InterpreterFor (AtomicState d !! e) r
interpretAtomicStateDb initial =
  interpretCrudSingleton .
  interpretStoreDb .
  interpretAtomicStateStore initial .
  raiseUnder2
{-# INLINE interpretAtomicStateDb #-}
