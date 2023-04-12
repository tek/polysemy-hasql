module Polysemy.Db.Effect.Store where

import Data.UUID (UUID)
import Lens.Micro.Extras (view)
import Sqel (Uid)

data QStore f q d :: Effect where
  Insert :: d -> QStore f i d m ()
  Upsert :: d -> QStore f i d m ()
  Delete :: i -> QStore f i d m (f d)
  DeleteAll :: QStore f i d m [d]
  Fetch :: i -> QStore f i d m (f d)
  FetchAll :: QStore f i d m [d]

makeSem ''QStore

type Store i d = QStore Maybe i (Uid i d)

type UuidStore d =
  Store UUID d

type family StoreEffects i e ds :: EffectRow where
  StoreEffects _ _ '[] = '[]
  StoreEffects i e (d : ds) = (Store i d !! e : StoreEffects i e ds)

type family Stores i e ds r :: Constraint where
  Stores _ _ '[] _ = ()
  Stores i e (d : ds) r = (Member (Store i d !! e) r, Stores i e ds r)

elem ::
  ∀ i d r .
  Member (Store i d) r =>
  i ->
  Sem r Bool
elem id' =
  isJust <$> fetch id'

fetchPayload ::
  ∀ i d r .
  Member (Store i d) r =>
  i ->
  Sem r (Maybe d)
fetchPayload id' =
  fmap (view #payload) <$> fetch id'

alter ::
  ∀ i d r .
  Member (Store i d) r =>
  i ->
  (d -> d) ->
  Sem r ()
alter id' f = do
  cur <- fetch id'
  traverse_ (upsert . (#payload %~ f)) cur
