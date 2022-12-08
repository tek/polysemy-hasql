module Polysemy.Db.Interpreter.Reader where

import Polysemy.Internal.Tactics (liftT)
import Polysemy.Reader (Reader (Ask, Local))

import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (QStore)

insertValue ::
  ∀ d e r .
  Members [QStore Maybe () d !! e, Stop e] r =>
  Sem r d ->
  Sem r d
insertValue initial =
  initial >>= tap \ d ->
    restop (Store.deleteAll *> Store.insert d)

readValue ::
  ∀ d e r .
  Members [QStore Maybe () d !! e, Stop e] r =>
  Sem r d ->
  Sem r d
readValue initial = do
  stored <- restop (Store.fetch ())
  maybe (insertValue @d @e initial) pure stored

-- |Interpret 'Reader' as a singleton table.
--
-- Given an initial value, every action reads the value from the database, potentially writing it on first access.
interpretReaderStore ::
  ∀ d e r .
  Member (QStore Maybe () d !! e) r =>
  Sem r d ->
  InterpreterFor (Reader d !! e) r
interpretReaderStore initial =
  interpretResumableH \case
    Ask -> do
      liftT (readValue @d @e (raise initial))
    Local f ma ->
      raise . interpretReaderStore (f <$> raise initial) =<< runT ma

interpretReaderStoreAs ::
  ∀ d e r .
  Member (QStore Maybe () d !! e) r =>
  d ->
  InterpreterFor (Reader d !! e) r
interpretReaderStoreAs initial =
  interpretReaderStore (pure initial)
