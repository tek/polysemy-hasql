module Polysemy.Db.Reader where

import Polysemy.Internal.Tactics (liftT)
import Polysemy.Reader (Reader (Ask, Local))
import Sqel.Data.Uid (Uid (Uid))

import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import qualified Polysemy.Db.Store as Store

insertValue ::
  ∀ d e r .
  Members [Store () d !! e, Stop e] r =>
  d ->
  Sem r d
insertValue initial =
  restop @e @(Store () d) do
    initial <$ (Store.deleteAll @() @d >> Store.insert @() @d (Uid () initial))

readValue ::
  ∀ d e r .
  Members [Store () d !! e, Stop e] r =>
  d ->
  Sem r d
readValue initial = do
  stored <- restop @e @(Store () d) (Store.fetchPayload @() @d ())
  maybe (insertValue @d @e initial) pure stored

-- |Interpret 'Reader' as a singleton table.
--
-- Given an initial value, every action reads the value from the database, potentially writing it on first access.
interpretReaderStore ::
  ∀ d e r .
  Member (Store () d !! e) r =>
  d ->
  InterpreterFor (Reader d !! e) r
interpretReaderStore initial =
  interpretResumableH \case
    Ask -> do
      liftT (readValue @d @e initial)
    Local f ma ->
      raise . interpretReaderStore (f initial) =<< runT ma
{-# inline interpretReaderStore #-}
