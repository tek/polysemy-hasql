module Polysemy.Db.Store where

import Control.Lens (view, views)

import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store(..), UidStore)
import qualified Polysemy.Db.Data.Uid as Uid
import Polysemy.Db.Data.Uid (Uid)

newtype StrictStore a =
  StrictStore {
    _records :: [a]
  }
  deriving (Eq, Show, Generic)
  deriving newtype(Default, Functor, Applicative)

makeClassy ''StrictStore

interpretStoreAtomicState ::
  ∀ i d r .
  Eq i =>
  (d -> i) ->
  Member (AtomicState (StrictStore d)) r =>
  InterpreterFor (Store i d) r
interpretStoreAtomicState getId =
  interpret \case
    Insert d ->
      atomicModify' @(StrictStore d) (over records (d :))
    Upsert d ->
      atomicModify' @(StrictStore d) (over records mod')
      where
        mod' a =
          d : filter (\ d' -> getId d /= getId d') a
    Delete id' -> do
      result <- atomicGets @(StrictStore d) (views records (filter ((id' ==) . getId)))
      nonEmpty result <$ atomicModify' @(StrictStore d) (records %~ (filter ((id' /=) . getId)))
    Fetch id' ->
      atomicGets @(StrictStore d) (views records (find ((id' ==) . getId)))
    FetchAll ->
      atomicGets @(StrictStore d) (nonEmpty . view records)

interpretStoreAtomicWith ::
  Eq i =>
  Member (Embed IO) r =>
  (d -> i) ->
  TVar (StrictStore d) ->
  InterpreterFor (Store i d) r
interpretStoreAtomicWith getId tvar sem =
  runAtomicStateTVar tvar . interpretStoreAtomicState getId . raiseUnder $ sem

interpretStoreAtomic ::
  Eq i =>
  Member (Embed IO) r =>
  (d -> i) ->
  StrictStore d ->
  InterpreterFor (Store i d) r
interpretStoreAtomic getId init' sem = do
  tvar <- newTVarIO init'
  interpretStoreAtomicWith getId tvar sem

interpretStoreUidAtomic ::
  ∀ d i r .
  Eq i =>
  Member (Embed IO) r =>
  StrictStore (Uid i d) ->
  InterpreterFor (Store i (Uid i d)) r
interpretStoreUidAtomic =
  interpretStoreAtomic Uid._id

interpretStoreStrictState ::
  ∀ i d r .
  Eq i =>
  Member (State (StrictStore d)) r =>
  (d -> i) ->
  InterpreterFor (Store i d) r
interpretStoreStrictState getId =
  interpret \case
    Insert d ->
      modify' @(StrictStore d) (over records (d :))
    Upsert d ->
      modify' @(StrictStore d) (over records mod')
      where
        mod' a =
          d : filter (\ d' -> getId d /= getId d') a
    Delete id' -> do
      result <- gets @(StrictStore d) (views records (filter ((id' ==) . getId)))
      nonEmpty result <$ modify' @(StrictStore d) (records %~ (filter ((id' /=) . getId)))
    Fetch id' ->
      gets @(StrictStore d) (views records (find ((id' ==) . getId)))
    FetchAll ->
      gets @(StrictStore d) $ nonEmpty . view records

interpretStoreStrict ::
  ∀ d i r .
  Eq i =>
  Member (Embed IO) r =>
  (d -> i) ->
  StrictStore d ->
  InterpreterFor (Store i d) r
interpretStoreStrict getId init' = do
  evalState init' . interpretStoreStrictState getId . raiseUnder

interpretStoreUidStrict ::
  ∀ d i r .
  Eq i =>
  Member (Embed IO) r =>
  StrictStore (Uid i d) ->
  InterpreterFor (Store i (Uid i d)) r
interpretStoreUidStrict =
  interpretStoreStrict Uid._id

interpretStoreNull ::
  InterpreterFor (Store i d) r
interpretStoreNull =
  interpret \case
    Insert _ ->
      unit
    Upsert _ ->
      unit
    Delete _ ->
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
  Member (UidStore i d) r =>
  i ->
  Sem r (Maybe d)
fetchPayload =
  fmap (fmap Uid._payload) . Store.fetch @i @(Uid i d)

-- TODO remove, seems pointless with 'Stop'
fetchPayloadAs ::
  ∀ i e' e d r .
  Members [UidStore i d ! e', Stop e] r =>
  (e' -> e) ->
  i ->
  Sem r (Maybe d)
fetchPayloadAs liftError id' =
  fmap Uid._payload <$> resumeHoist @_ @_ @(UidStore i d) liftError (Store.fetch @i @(Uid i d) id')

fetchPayloadShow ::
  ∀ i e' e d r .
  Show e' =>
  Members [UidStore i d ! e', Stop e] r =>
  (Text -> e) ->
  i ->
  Sem r (Maybe d)
fetchPayloadShow liftError id' =
  fmap Uid._payload <$> (resumeHoist @e' @_ @(UidStore i d) (liftError . show) (Store.fetch @i @(Uid i d) id'))

alter ::
  ∀ i d r .
  Member (Store i d) r =>
  i ->
  (d -> d) ->
  Sem r ()
alter id' f = do
  cur <- Store.fetch @i id'
  traverse_ (Store.upsert @i . f) cur

alterOr ::
  ∀ i d r .
  Member (Store i d) r =>
  d ->
  i ->
  (d -> d) ->
  Sem r ()
alterOr fallback id' f = do
  cur <- Store.fetch @i id'
  Store.upsert @i (f (fromMaybe fallback cur))

alterDefault ::
  ∀ i d r .
  Member (Store i d) r =>
  Default d =>
  i ->
  (d -> d) ->
  Sem r ()
alterDefault =
  alterOr @i def
