module Polysemy.Db.Store where

import Control.Lens (view, views)

import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store(..), UidStore)
import Polysemy.Db.Data.StoreError (StoreError)
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
  ∀ i d e r .
  Eq i =>
  (d -> i) ->
  Member (AtomicState (StrictStore d)) r =>
  InterpreterFor (Store i e d) r
interpretStoreAtomicState getId =
  interpret \case
    Insert d ->
      Right <$> atomicModify' @(StrictStore d) (over records (d :))
    Upsert d ->
      Right <$> atomicModify' @(StrictStore d) (over records mod')
      where
        mod' a =
          d : filter (\ d' -> getId d /= getId d') a
    Delete id' ->
      Right <$> atomicModify' @(StrictStore d) (records %~ (filter ((id' /=) . getId)))
    Fetch id' ->
      atomicGets @(StrictStore d) (Right . views records (find ((id' ==) . getId)))
    FetchAll ->
      atomicGets @(StrictStore d) $ Right . nonEmpty . view records

interpretStoreAtomicWith ::
  Eq i =>
  Member (Embed IO) r =>
  (d -> i) ->
  TVar (StrictStore d) ->
  InterpreterFor (Store i () d) r
interpretStoreAtomicWith getId tvar sem =
  runAtomicStateTVar tvar . interpretStoreAtomicState getId . raiseUnder $ sem

interpretStoreAtomic ::
  Eq i =>
  Member (Embed IO) r =>
  (d -> i) ->
  StrictStore d ->
  InterpreterFor (Store i () d) r
interpretStoreAtomic getId init' sem = do
  tvar <- newTVarIO init'
  interpretStoreAtomicWith getId tvar sem

interpretStoreStrictState ::
  ∀ i d e r .
  Eq i =>
  Member (State (StrictStore d)) r =>
  (d -> i) ->
  InterpreterFor (Store i e d) r
interpretStoreStrictState getId =
  interpret \case
    Insert d ->
      Right <$> modify' @(StrictStore d) (over records (d :))
    Upsert d ->
      Right <$> modify' @(StrictStore d) (over records mod')
      where
        mod' a =
          d : filter (\ d' -> getId d /= getId d') a
    Delete id' ->
      Right <$> modify' @(StrictStore d) (records %~ (filter ((id' /=) . getId)))
    Fetch id' ->
      gets @(StrictStore d) (Right . views records (find ((id' ==) . getId)))
    FetchAll ->
      gets @(StrictStore d) $ Right . nonEmpty . view records

interpretStoreStrict ::
  ∀ d i r .
  Eq i =>
  Member (Embed IO) r =>
  (d -> i) ->
  StrictStore d ->
  InterpreterFor (Store i () d) r
interpretStoreStrict getId init' = do
  evalState init' . interpretStoreStrictState getId . raiseUnder

interpretStoreUidStrict ::
  ∀ d i r .
  Eq i =>
  Eq d =>
  Member (Embed IO) r =>
  StrictStore (Uid i d) ->
  InterpreterFor (Store i () (Uid i d)) r
interpretStoreUidStrict init' = do
  evalState init' . interpretStoreStrictState Uid._id . raiseUnder

interpretStoreNull ::
  InterpreterFor (Store i e d) r
interpretStoreNull =
  interpret \case
    Insert _ ->
      pure (Right ())
    Upsert _ ->
      pure (Right ())
    Delete _ ->
      pure (Right ())
    Fetch _ ->
      pure (Right Nothing)
    FetchAll ->
      pure (Right Nothing)

elem ::
  ∀ i e d r .
  Members [Store i e d, Error (StoreError e)] r =>
  i ->
  Sem r Bool
elem id' =
  fromEither =<< (fmap isJust <$> Store.fetch @i @e @d id')

fetch ::
  ∀ i e d r .
  Members [Store i e d, Error (StoreError e)] r =>
  i ->
  Sem r (Maybe d)
fetch =
  fromEither <=< Store.fetch @i @e @d

fetchPayload ::
  ∀ i e d r .
  Members [UidStore i e d, Error (StoreError e)] r =>
  i ->
  Sem r (Maybe d)
fetchPayload =
  fmap (fmap Uid._payload) . fetch @i @e @(Uid i d)

fetchPayloadAs ::
  ∀ i e' e d r .
  Members [UidStore i e' d, Error e] r =>
  (StoreError e' -> e) ->
  i ->
  Sem r (Maybe d)
fetchPayloadAs liftError id' =
  fmap Uid._payload <$> (hoistEither liftError =<< Store.fetch @i @e' @(Uid i d) id')

fetchPayloadShow ::
  ∀ i e' e d r .
  Show e' =>
  Members [UidStore i e' d, Error e] r =>
  (Text -> e) ->
  i ->
  Sem r (Maybe d)
fetchPayloadShow liftError id' =
  fmap Uid._payload <$> (hoistEitherShow liftError =<< Store.fetch @i @e' @(Uid i d) id')

insert ::
  ∀ i e d r .
  Members [Store i e d, Error (StoreError e)] r =>
  d ->
  Sem r ()
insert =
  fromEither <=< Store.insert @i @e @d

upsert ::
  ∀ i e d r .
  Members [Store i e d, Error (StoreError e)] r =>
  d ->
  Sem r ()
upsert =
  fromEither <=< Store.upsert @i @e @d

delete ::
  ∀ i e d r .
  Members [Store i e d, Error (StoreError e)] r =>
  i ->
  Sem r ()
delete =
  fromEither <=< Store.delete @i @e @d
