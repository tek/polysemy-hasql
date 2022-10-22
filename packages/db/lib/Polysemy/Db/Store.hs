module Polysemy.Db.Store where

import Control.Concurrent.STM.TVar (TVar)
import Control.Lens (makeClassy, view, views)
import Prelude hiding (type (@@))

import Polysemy.Db.Atomic (interpretAtomic)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store (..))
import qualified Polysemy.Db.Data.Uid as Uid
import Polysemy.Db.Data.Uid (Uid (Uid))
import Polysemy.Db.Tree.Data (GenDataTree, ReifyDataTree)
import Polysemy.Db.Tree.Partial (UpdatePartialTree)
import Polysemy.Db.Tree.Partial.Insert (InsertPaths)

type PureStoreUpdate d fields tree dataTree =
  (
    GenDataTree d dataTree,
    ReifyDataTree dataTree d,
    UpdatePartialTree dataTree tree,
    InsertPaths d fields tree
  )

type UidPureStoreUpdate i d fields tree dataTree =
  PureStoreUpdate (Uid i d) fields tree dataTree

newtype PureStore a =
  PureStore {
    _records :: [a]
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Default, Semigroup, Monoid)

makeClassy ''PureStore

type PureUidStore i d =
  PureStore (Uid i d)

interpretStoreAtomicState ::
  ∀ i d e r .
  Eq i =>
  Member (AtomicState (PureUidStore i d)) r =>
  InterpreterFor (Store i d !! e) r
interpretStoreAtomicState =
  interpretResumable \case
    Insert d ->
      atomicModify' @(PureUidStore i d) (records %~ (d :))
    Upsert d ->
      atomicModify' @(PureUidStore i d) (records %~ updateRecords)
      where
        updateRecords a =
          d : filter (\ d' -> Uid._id d /= Uid._id d') a
    Delete id' -> do
      result <- atomicGets @(PureUidStore i d) (views records (filter ((id' ==) . Uid._id)))
      nonEmpty result <$ atomicModify' @(PureUidStore i d) (records %~ (filter ((id' /=) . Uid._id)))
    DeleteAll -> do
      atomicState' @(PureUidStore i d) \ (PureStore ds) -> (mempty, nonEmpty ds)
    Fetch id' ->
      atomicGets @(PureUidStore i d) (views records (find ((id' ==) . Uid._id)))
    FetchAll ->
      atomicGets @(PureUidStore i d) (nonEmpty . view records)

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
      modify' @(PureUidStore i d) (records %~ (d :))
    Upsert d ->
      modify' @(PureUidStore i d) (records %~ updateRecords)
      where
        updateRecords a =
          d : filter (\ d' -> Uid._id d /= Uid._id d') a
    Delete id' -> do
      result <- gets @(PureUidStore i d) (views records (filter ((id' ==) . Uid._id)))
      nonEmpty result <$ modify' @(PureUidStore i d) (records %~ (filter ((id' /=) . Uid._id)))
    DeleteAll -> do
      gets @(PureUidStore i d) (nonEmpty . _records) <* put @(PureUidStore i d) mempty
    Fetch id' ->
      gets @(PureUidStore i d) (views records (find ((id' ==) . Uid._id)))
    FetchAll ->
      gets @(PureUidStore i d) $ nonEmpty . view records

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
      pure Nothing
    Fetch _ ->
      pure Nothing
    FetchAll ->
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
