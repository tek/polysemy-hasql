module Polysemy.Db.AtomicState where

import Polysemy.AtomicState (AtomicState(AtomicGet, AtomicState))
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)

insertState ::
  Members [Store () s !! e, Stop e] r =>
  s ->
  Sem r s
insertState initial =
  restop do
    initial <$ (Store.deleteAll >> Store.insert initial)

readState ::
  Members [Store () s !! e, Stop e] r =>
  s ->
  Sem r s
readState initial = do
  stored <- restop (Store.fetch ())
  maybe (insertState initial) pure stored

-- |Interpret 'AtomicState' as a singleton table.
--
-- Given an initial value, every state action reads the value from the database and writes it back.
interpretAtomicStateStore ::
  Member (Store () d !! e) r =>
  d ->
  InterpreterFor (AtomicState d !! e) r
interpretAtomicStateStore initial =
  interpretResumable \case
    AtomicState f -> do
      (newState, a) <- f <$> readState initial
      a <$ insertState newState
    AtomicGet ->
      readState initial
{-# INLINE interpretAtomicStateStore #-}
