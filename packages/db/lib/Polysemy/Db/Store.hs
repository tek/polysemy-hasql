module Polysemy.Db.Store where

import Control.Lens (view, views)
import Polysemy.AtomicState (atomicState')

import Polysemy.Db.Atomic (interpretAtomic)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store(..), UidStore)
import qualified Polysemy.Db.Data.Uid as Uid
import Polysemy.Db.Data.Uid (Uid)

newtype StrictStore a =
  StrictStore {
    _records :: [a]
  }
  deriving (Eq, Show, Generic)
  deriving newtype (Default, Functor, Applicative, Semigroup, Monoid)

makeClassy ''StrictStore

interpretStoreAtomicState ::
  ∀ i d e r .
  Eq i =>
  (d -> i) ->
  Member (AtomicState (StrictStore d)) r =>
  InterpreterFor (Store i d !! e) r
interpretStoreAtomicState getId =
  interpretResumable \case
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
    DeleteAll -> do
      atomicState' @(StrictStore d) \ (StrictStore ds) -> (mempty, nonEmpty ds)
    Fetch id' ->
      atomicGets @(StrictStore d) (views records (find ((id' ==) . getId)))
    FetchAll ->
      atomicGets @(StrictStore d) (nonEmpty . view records)

-- |This is a blackbox interpreter that takes an initial list of records and stores them in an 'AtomicState'.
-- The @getId@ parameter is used to extract the query id from the record type.
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
  (d -> i) ->
  StrictStore d ->
  InterpreterFor (Store i d !! e) r
interpretStoreAtomic getId init' =
  interpretAtomic init' . interpretStoreAtomicState getId . raiseUnder

interpretStoreUidAtomic ::
  ∀ i d e r .
  Eq i =>
  Member (Embed IO) r =>
  StrictStore (Uid i d) ->
  InterpreterFor (Store i (Uid i d) !! e) r
interpretStoreUidAtomic =
  interpretStoreAtomic Uid._id

interpretStoreStrictState ::
  ∀ i d e r .
  Eq i =>
  Member (State (StrictStore d)) r =>
  (d -> i) ->
  InterpreterFor (Store i d !! e) r
interpretStoreStrictState getId =
  interpretResumable \case
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
    DeleteAll -> do
      gets @(StrictStore d) (nonEmpty . _records) <* put @(StrictStore d) mempty
    Fetch id' ->
      gets @(StrictStore d) (views records (find ((id' ==) . getId)))
    FetchAll ->
      gets @(StrictStore d) $ nonEmpty . view records

interpretStoreTVar ::
  Eq i =>
  Member (Embed IO) r =>
  (d -> i) ->
  TVar (StrictStore d) ->
  InterpreterFor (Store i d !! e) r
interpretStoreTVar getId tvar =
  runAtomicStateTVar tvar .
  interpretStoreAtomicState getId .
  raiseUnder

interpretStoreStrict ::
  ∀ i d e r .
  Eq i =>
  Member (Embed IO) r =>
  (d -> i) ->
  StrictStore d ->
  InterpreterFor (Store i d !! e) r
interpretStoreStrict getId init' =
  evalState init' .
  interpretStoreStrictState getId .
  raiseUnder

interpretStoreUidStrict ::
  ∀ i d e r .
  Eq i =>
  Member (Embed IO) r =>
  StrictStore (Uid i d) ->
  InterpreterFor (Store i (Uid i d) !! e) r
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
  Member (UidStore i d) r =>
  i ->
  Sem r (Maybe d)
fetchPayload id' =
  fmap Uid._payload <$> Store.fetch @i @(Uid i d) id'

fetchPayloadShow ::
  ∀ i e' e d r .
  Show e' =>
  Members [UidStore i d !! e', Stop e] r =>
  (Text -> e) ->
  i ->
  Sem r (Maybe d)
fetchPayloadShow liftError id' =
  fmap Uid._payload <$> (resumeHoist @e' @(UidStore i d) (liftError . show) (Store.fetch @i @(Uid i d) id'))

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
