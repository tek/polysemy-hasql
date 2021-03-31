module Polysemy.Db.Reader where

import Polysemy.Internal.Tactics (runT, liftT)
import Polysemy.Reader (Reader(Ask, Local))

import Polysemy.Db.AtomicState (readState)
import Polysemy.Db.Data.Store (Store)

-- |Interpret 'Reader' as a singleton table.
--
-- Given an initial value, every state action reads the value from the database and writes it back.
interpretReaderStore ::
  ∀ d e r .
  Member (Store () d !! e) r =>
  d ->
  InterpreterFor (Reader d !! e) r
interpretReaderStore initial =
  interpretResumableH \case
    Ask -> do
      liftT (readState @d @e initial)
    Local f ma ->
      raise . interpretReaderStore (f initial) =<< runT ma
{-# INLINE interpretReaderStore #-}
