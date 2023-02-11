module Polysemy.Db.AtomicState where

import Conc (Lock, lock)
import Polysemy.AtomicState (AtomicState (AtomicGet, AtomicState))

import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (QStore)

insertState ::
  ∀ d err r .
  Members [QStore Maybe () d !! err, Stop err] r =>
  Sem r d ->
  Sem r d
insertState initial = do
  restop do
    raise initial >>= tap \ d ->
      Store.deleteAll *> Store.insert d

readState ::
  ∀ d err r .
  Members [QStore Maybe () d !! err, Stop err] r =>
  Sem r d ->
  Sem r d
readState initial = do
  stored <- restop (Store.fetch ())
  maybe (insertState @d @err initial) pure stored

handleAtomicStateStore ::
  ∀ tag d err r0 r a .
  Members [QStore Maybe () d !! err, Lock @@ tag, Stop err] r =>
  Sem r d ->
  AtomicState d (Sem r0) a ->
  Sem r a
handleAtomicStateStore initial = \case
  AtomicState f ->
    tag @tag @Lock $ lock do
      (newState, a) <- f <$> raise (readState @d @err initial)
      a <$ insertState @d @err (pure newState)
  AtomicGet ->
    readState @d @err initial

-- |Interpret 'AtomicState' as a singleton table.
--
-- Given an action that produces an initial value, every action reads the value from the database and writes it
-- back.
interpretAtomicStateStore ::
  ∀ tag d err r .
  Members [QStore Maybe () d !! err, Lock @@ tag] r =>
  Sem (Stop err : r) d ->
  InterpreterFor (AtomicState d !! err) r
interpretAtomicStateStore initial =
  interpretResumable (handleAtomicStateStore initial)

-- |Interpret 'AtomicState' as a singleton table.
--
-- Given an initial value, every action reads the value from the database and writes it back.
interpretAtomicStateStoreAs ::
  ∀ tag d err r .
  Members [QStore Maybe () d !! err, Lock @@ tag] r =>
  d ->
  InterpreterFor (AtomicState d !! err) r
interpretAtomicStateStoreAs value =
  interpretAtomicStateStore (pure value)

atomicStateScope ::
  param ->
  Member (Scoped param (QStore Maybe () d !! err) !! err) r =>
  (() -> Sem (QStore Maybe () d !! err : Stop err : r) a) ->
  Sem (Stop err : r) a
atomicStateScope p use =
  restop (scoped p (raiseUnder (use ())))

interpretAtomicStateStoreScoped ::
  ∀ tag param d err r .
  Members [Scoped param (QStore Maybe () d !! err) !! err, Lock @@ tag] r =>
  Sem (Stop err : r) d ->
  InterpreterFor (Scoped param (AtomicState d !! err) !! err) r
interpretAtomicStateStoreScoped initial =
  interpretScopedRWith @'[QStore Maybe () d !! err] atomicStateScope \ () ->
    handleAtomicStateStore (insertAt @1 initial)

interpretAtomicStateStoreScopedAs ::
  ∀ tag param d err r .
  Members [Scoped param (QStore Maybe () d !! err) !! err, Lock @@ tag] r =>
  d ->
  InterpreterFor (Scoped param (AtomicState d !! err) !! err) r
interpretAtomicStateStoreScopedAs value =
  interpretAtomicStateStoreScoped (pure value)

interpretAtomicStatesStore ::
  ∀ tag param d err r .
  Members [QStore Maybe () d !! err, Scoped param (QStore Maybe () d !! err) !! err, Lock @@ tag] r =>
  Sem (Stop err : r) d ->
  InterpretersFor [AtomicState d !! err, Scoped param (AtomicState d !! err) !! err] r
interpretAtomicStatesStore initial =
  interpretAtomicStateStoreScoped initial .
  interpretAtomicStateStore (raiseUnder initial)
