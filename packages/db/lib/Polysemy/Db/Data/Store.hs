module Polysemy.Db.Data.Store where

import Polysemy.Db.Data.Uid (Uid, Uuid)

data Store p d :: Effect where
  Insert :: d -> Store p d m ()
  Upsert :: d -> Store p d m ()
  Delete :: p -> Store p d m ()
  Fetch :: p -> Store p d m (Maybe d)
  FetchAll :: Store p d m (Maybe (NonEmpty d))

makeSem ''Store

type UidStore i d =
  Store i (Uid i d)

type UuidStore d =
  Store UUID (Uuid d)

type family StoreEffects i ds :: EffectRow where
  StoreEffects _ '[] = '[]
  StoreEffects i (d : ds) = (UidStore i d : StoreEffects i ds)

type family Stores i ds r :: Constraint where
  Stores _ '[] _ = ()
  Stores i (d : ds) r = (Member (UidStore i d) r, Stores i ds r)
