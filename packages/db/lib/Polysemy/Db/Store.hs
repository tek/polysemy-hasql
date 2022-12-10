module Polysemy.Db.Store where

import Conc (interpretAtomic)
import Control.Concurrent.STM.TVar (TVar)
import Control.Lens (view, views)
import Prelude hiding (type (@@))
import qualified Sqel.Data.Uid as Uid
import Sqel.Data.Uid (Uid (Uid))

import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (QStore (..), Store)

newtype PureStore a =
  PureStore {
    records :: [a]
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Default, Semigroup, Monoid)

type PureUidStore i d =
  PureStore (Uid i d)

-- TODO make this correct, especially deleting
interpretStoreAtomicState ::
  ∀ i d e r .
  Eq i =>
  Member (AtomicState (PureUidStore i d)) r =>
  InterpreterFor (Store i d !! e) r
interpretStoreAtomicState =
  interpretResumable \case
    Insert d ->
      atomicModify' @(PureUidStore i d) (#records %~ (d :))
    Upsert d ->
      atomicModify' @(PureUidStore i d) (#records %~ updateRecords)
      where
        updateRecords a =
          d : filter (\ d' -> Uid.id d /= Uid.id d') a
    Delete id' -> do
      result <- atomicGets @(PureUidStore i d) (views #records (filter ((id' ==) . Uid.id)))
      listToMaybe result <$ atomicModify' @(PureUidStore i d) (#records %~ (filter ((id' /=) . Uid.id)))
    DeleteAll -> do
      atomicState' @(PureUidStore i d) \ (PureStore ds) -> (mempty, ds)
    Fetch id' ->
      atomicGets @(PureUidStore i d) (views #records (find ((id' ==) . Uid.id)))
    FetchAll ->
      atomicGets @(PureUidStore i d) (view #records)

-- |This is a blackbox interpreter that takes an initial list of records and stores them in an 'AtomicState'.
--
-- This interpreter uses 'Resumable', which is a more restrictive version of 'Polysemy.Error'.
--
-- Given the program shown above:
--
-- >>> runM (runStop (runAsResumable (interpretStoreAtomic User.id mempty progStore)))
-- Right (Just (User 1 "root"))
interpretStoreAtomic ::
  ∀ i d e r .
  Eq i =>
  Member (Embed IO) r =>
  PureUidStore i d ->
  InterpreterFor (Store i d !! e) r
interpretStoreAtomic init' =
  interpretAtomic init' . interpretStoreAtomicState . raiseUnder

interpretStorePureState ::
  ∀ i d e r .
  Eq i =>
  Member (State (PureUidStore i d)) r =>
  InterpreterFor (Store i d !! e) r
interpretStorePureState =
  interpretResumable \case
    Insert d ->
      modify' @(PureUidStore i d) (#records %~ (d :))
    Upsert d ->
      modify' @(PureUidStore i d) (#records %~ updateRecords)
      where
        updateRecords a =
          d : filter (\ d' -> Uid.id d /= Uid.id d') a
    Delete id' -> do
      result <- gets @(PureUidStore i d) (views #records (filter ((id' ==) . Uid.id)))
      listToMaybe result <$ modify' @(PureUidStore i d) (#records %~ (filter ((id' /=) . Uid.id)))
    DeleteAll -> do
      gets @(PureUidStore i d) records <* put @(PureUidStore i d) mempty
    Fetch id' ->
      gets @(PureUidStore i d) (views #records (find ((id' ==) . Uid.id)))
    FetchAll ->
      gets @(PureUidStore i d) $ view #records

interpretStoreTVar ::
  Eq i =>
  Member (Embed IO) r =>
  TVar (PureUidStore i d) ->
  InterpreterFor (Store i d !! e) r
interpretStoreTVar tvar =
  runAtomicStateTVar tvar .
  interpretStoreAtomicState .
  raiseUnder

interpretStorePure ::
  ∀ i d e r .
  Eq i =>
  PureUidStore i d ->
  InterpreterFor (Store i d !! e) r
interpretStorePure init' =
  evalState init' .
  interpretStorePureState .
  raiseUnder

interpretStoreNull ::
  InterpreterFor (Store i d !! e) r
interpretStoreNull =
  interpretResumable \case
    Insert _ ->
      unit
    Upsert _ ->
      unit
    Delete _ ->
      pure Nothing
    DeleteAll ->
      pure []
    Fetch _ ->
      pure Nothing
    FetchAll ->
      pure []

elem ::
  ∀ i d r .
  Member (Store i d) r =>
  i ->
  Sem r Bool
elem id' =
  isJust <$> Store.fetch id'

fetchPayload ::
  ∀ i d r .
  Member (Store i d) r =>
  i ->
  Sem r (Maybe d)
fetchPayload id' =
  fmap Uid.payload <$> Store.fetch id'

fetchPayloadShow ::
  ∀ i e' e d r .
  Show e' =>
  Members [Store i d !! e', Stop e] r =>
  (Text -> e) ->
  i ->
  Sem r (Maybe d)
fetchPayloadShow liftError id' =
  fmap Uid.payload <$> (resumeHoist @e' @(Store i d) (liftError . show) (Store.fetch id'))

alter ::
  ∀ i d r .
  Member (Store i d) r =>
  i ->
  (d -> d) ->
  Sem r ()
alter id' f = do
  cur <- Store.fetch id'
  traverse_ (Store.upsert . (#payload %~ f)) cur

alterOr ::
  ∀ i d r .
  Member (Store i d) r =>
  d ->
  i ->
  (d -> d) ->
  Sem r ()
alterOr fallback id' f = do
  cur <- Store.fetch id'
  Store.upsert ((#payload %~ f) (fromMaybe (Uid id' fallback) cur))

alterDefault ::
  ∀ i d r .
  Member (Store i d) r =>
  Default d =>
  i ->
  (d -> d) ->
  Sem r ()
alterDefault =
  alterOr @i def
