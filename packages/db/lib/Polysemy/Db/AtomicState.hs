module Polysemy.Db.AtomicState where

import Polysemy.AtomicState (AtomicState(AtomicGet, AtomicState))
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)

insertState ::
  ∀ d e r .
  Members [Store () d !! e, Stop e] r =>
  d ->
  Sem r d
insertState initial =
  restop @e @(Store () d) do
    initial <$ (Store.deleteAll @() @d >> Store.insert @() @d initial)

readState ::
  ∀ d e r .
  Members [Store () d !! e, Stop e] r =>
  d ->
  Sem r d
readState initial = do
  stored <- restop @e @(Store () d) (Store.fetch @() @d ())
  maybe (insertState @d @e initial) pure stored

-- |Interpret 'AtomicState' as a singleton table.
--
-- Given an initial value, every state action reads the value from the database and writes it back.
interpretAtomicStateStore ::
  ∀ d e r .
  Member (Store () d !! e) r =>
  d ->
  InterpreterFor (AtomicState d !! e) r
interpretAtomicStateStore initial =
  interpretResumable \case
    AtomicState f -> do
      (newState, a) <- f <$> readState @d @e initial
      a <$ insertState @d @e newState
    AtomicGet ->
      readState @d @e initial
{-# INLINE interpretAtomicStateStore #-}
