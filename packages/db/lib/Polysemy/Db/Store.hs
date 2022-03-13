module Polysemy.Db.Store where

import Control.Concurrent.STM.TVar (TVar)
import Control.Lens (makeClassy, view, views, (%~))
import Data.Composition ((.:))

import Polysemy.Db.Atomic (interpretAtomic)
import Polysemy.Db.Data.Partial (Partial, getPartial)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store (..))
import qualified Polysemy.Db.Data.Uid as Uid
import Polysemy.Db.Data.Uid (Uid (Uid))
import Polysemy.Db.Tree.Data (GenDataTree, ReifyDataTree)
import Polysemy.Db.Tree.Partial (UpdatePartialTree, updatePartial)
import Polysemy.Db.Tree.Partial.Insert (InsertPaths)

type StrictStoreUpdate d fields tree dataTree =
  (
    GenDataTree d dataTree,
    ReifyDataTree dataTree d,
    UpdatePartialTree dataTree tree,
    InsertPaths d fields tree
  )

type UidStrictStoreUpdate i d fields tree dataTree =
  StrictStoreUpdate (Uid i d) fields tree dataTree

newtype StrictStore a =
  StrictStore {
    _records :: [a]
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Default, Semigroup, Monoid)

makeClassy ''StrictStore

type StrictUidStore i d =
  StrictStore (Uid i d)

interpretStoreAtomicState ::
  ∀ i d e r tree dataTree .
  Eq i =>
  StrictStoreUpdate d '[] tree dataTree =>
  Member (AtomicState (StrictUidStore i d)) r =>
  InterpreterFor (Store i d !! e) r
interpretStoreAtomicState =
  interpretResumable \case
    Insert d ->
      atomicModify' @(StrictUidStore i d) (records %~ (d :))
    Upsert d ->
      atomicModify' @(StrictUidStore i d) (records %~ updateRecords)
      where
        updateRecords a =
          d : filter (\ d' -> Uid._id d /= Uid._id d') a
    Delete id' -> do
      result <- atomicGets @(StrictUidStore i d) (views records (filter ((id' ==) . Uid._id)))
      nonEmpty result <$ atomicModify' @(StrictUidStore i d) (records %~ (filter ((id' /=) . Uid._id)))
    DeleteAll -> do
      atomicState' @(StrictUidStore i d) \ (StrictStore ds) -> (mempty, nonEmpty ds)
    Fetch id' ->
      atomicGets @(StrictUidStore i d) (views records (find ((id' ==) . Uid._id)))
    FetchAll ->
      atomicGets @(StrictUidStore i d) (nonEmpty . view records)
    Update i patch ->
      atomicGets @(StrictUidStore i d) (views records (find ((i ==) . Uid._id))) >>= \case
        Just d ->
          Just updated <$ atomicModify' @(StrictUidStore i d) (records %~ updateRecords)
          where
            updateRecords a =
              updated : filter (\ d' -> Uid._id d /= Uid._id d') a
            updated =
              fmap (updatePartial (getPartial patch)) d
        Nothing -> pure Nothing

-- |This is a blackbox interpreter that takes an initial list of records and stores them in an 'AtomicState'.
-- The @Uid._id@ parameter is used to extract the query id from the record type.
--
-- This interpreter uses 'Resumable', which is a more restrictive version of 'Polysemy.Error'.
--
-- Given the program shown above:
--
-- >>> runM (runStop (runAsResumable (interpretStoreAtomic User.id mempty progStore)))
-- Right (Just (User 1 "root"))
interpretStoreAtomic ::
  ∀ i d e r tree dataTree .
  Eq i =>
  Member (Embed IO) r =>
  StrictStoreUpdate d '[] tree dataTree =>
  StrictUidStore i d ->
  InterpreterFor (Store i d !! e) r
interpretStoreAtomic init' =
  interpretAtomic init' . interpretStoreAtomicState . raiseUnder

interpretStoreStrictState ::
  ∀ i d e r tree dataTree .
  Eq i =>
  StrictStoreUpdate d '[] tree dataTree =>
  Member (State (StrictUidStore i d)) r =>
  InterpreterFor (Store i d !! e) r
interpretStoreStrictState =
  interpretResumable \case
    Insert d ->
      modify' @(StrictUidStore i d) (records %~ (d :))
    Upsert d ->
      modify' @(StrictUidStore i d) (records %~ updateRecords)
      where
        updateRecords a =
          d : filter (\ d' -> Uid._id d /= Uid._id d') a
    Delete id' -> do
      result <- gets @(StrictUidStore i d) (views records (filter ((id' ==) . Uid._id)))
      nonEmpty result <$ modify' @(StrictUidStore i d) (records %~ (filter ((id' /=) . Uid._id)))
    DeleteAll -> do
      gets @(StrictUidStore i d) (nonEmpty . _records) <* put @(StrictUidStore i d) mempty
    Fetch id' ->
      gets @(StrictUidStore i d) (views records (find ((id' ==) . Uid._id)))
    FetchAll ->
      gets @(StrictUidStore i d) $ nonEmpty . view records
    Update i patch ->
      gets @(StrictUidStore i d) (views records (find ((i ==) . Uid._id))) >>= \case
        Just d ->
          Just updated <$ modify' @(StrictUidStore i d) (records %~ updateRecords)
          where
            updateRecords a =
              updated : filter (\ d' -> Uid._id d /= Uid._id d') a
            updated =
              fmap (updatePartial (getPartial patch)) d
        Nothing -> pure Nothing

interpretStoreTVar ::
  Eq i =>
  Member (Embed IO) r =>
  StrictStoreUpdate d '[] tree dataTree =>
  TVar (StrictUidStore i d) ->
  InterpreterFor (Store i d !! e) r
interpretStoreTVar tvar =
  runAtomicStateTVar tvar .
  interpretStoreAtomicState .
  raiseUnder

interpretStoreStrict ::
  ∀ i d e r tree dataTree .
  Eq i =>
  StrictStoreUpdate d '[] tree dataTree =>
  StrictUidStore i d ->
  InterpreterFor (Store i d !! e) r
interpretStoreStrict init' =
  evalState init' .
  interpretStoreStrictState .
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
      pure Nothing
    Fetch _ ->
      pure Nothing
    FetchAll ->
      pure Nothing
    Update _ _ ->
      pure Nothing

elem ::
  ∀ i d r .
  Member (Store i d) r =>
  i ->
  Sem r Bool
elem id' =
  isJust <$> Store.fetch @i @d id'

fetchPayload ::
  ∀ i d r .
  Member (Store i d) r =>
  i ->
  Sem r (Maybe d)
fetchPayload id' =
  fmap Uid._payload <$> Store.fetch @i @d id'

fetchPayloadShow ::
  ∀ i e' e d r .
  Show e' =>
  Members [Store i d !! e', Stop e] r =>
  (Text -> e) ->
  i ->
  Sem r (Maybe d)
fetchPayloadShow liftError id' =
  fmap Uid._payload <$> (resumeHoist @e' @(Store i d) (liftError . show) (Store.fetch @i @d id'))

alter ::
  ∀ i d r .
  Member (Store i d) r =>
  i ->
  (d -> d) ->
  Sem r ()
alter id' f = do
  cur <- Store.fetch @i @d id'
  traverse_ (Store.upsert @i . (Uid.payload %~ f)) cur

alterOr ::
  ∀ i d r .
  Member (Store i d) r =>
  d ->
  i ->
  (d -> d) ->
  Sem r ()
alterOr fallback id' f = do
  cur <- Store.fetch @i id'
  Store.upsert @i ((Uid.payload %~ f) (fromMaybe (Uid id' fallback) cur))

alterDefault ::
  ∀ i d r .
  Member (Store i d) r =>
  Default d =>
  i ->
  (d -> d) ->
  Sem r ()
alterDefault =
  alterOr @i def

update_ ::
  ∀ i d r .
  Member (Store i d) r =>
  i ->
  Partial d ->
  Sem r ()
update_ =
  void .: Store.update
