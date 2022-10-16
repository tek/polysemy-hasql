module Polysemy.Db.AtomicState where

import Polysemy.AtomicState (AtomicState (AtomicGet, AtomicState))

import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import qualified Polysemy.Db.Store as Store
import Conc (Lock, lock)

insertState ::
  ∀ d e r .
  Members [Store () d !! e, Stop e] r =>
  Sem r d ->
  Sem r d
insertState initial = do
  restop @e @(Store () d) do
    d <- raise initial
    d <$ (Store.deleteAll @() @d >> Store.insert @() @d (pure d))

readState ::
  ∀ d e r .
  Members [Store () d !! e, Stop e] r =>
  Sem r d ->
  Sem r d
readState initial = do
  stored <- restop @e @(Store () d) (Store.fetchPayload @() @d ())
  maybe (insertState @d @e initial) pure stored

-- |Interpret 'AtomicState' as a singleton table.
--
-- Given an action that produces an initial value, every action reads the value from the database and writes it
-- back.
interpretAtomicStateStore ::
  ∀ tag d e r .
  Members [Store () d !! e, Lock @@ tag] r =>
  Sem (Stop e : r) d ->
  InterpreterFor (AtomicState d !! e) r
interpretAtomicStateStore initial =
  interpretResumable \case
    AtomicState f ->
      tag @tag @Lock $ lock do
        (newState, a) <- f <$> raise (readState @d @e initial)
        a <$ insertState @d @e (pure newState)
    AtomicGet ->
      readState @d @e initial
{-# inline interpretAtomicStateStore #-}

-- |Interpret 'AtomicState' as a singleton table.
--
-- Given an initial value, every action reads the value from the database and writes it back.
interpretAtomicStateStoreAs ::
  ∀ tag d e r .
  Members [Store () d !! e, Lock @@ tag] r =>
  d ->
  InterpreterFor (AtomicState d !! e) r
interpretAtomicStateStoreAs value =
  interpretAtomicStateStore (pure value)
{-# inline interpretAtomicStateStoreAs #-}
