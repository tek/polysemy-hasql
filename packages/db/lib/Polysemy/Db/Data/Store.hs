module Polysemy.Db.Data.Store where

import Polysemy.Db.Data.Uid (Uid, Uuid)

data Store p d :: Effect where
  Insert :: d -> Store p d m ()
  Upsert :: d -> Store p d m ()
  Delete :: p -> Store p d m (Maybe (NonEmpty d))
  Fetch :: p -> Store p d m (Maybe d)
  FetchAll :: Store p d m (Maybe (NonEmpty d))

makeSem ''Store

type UidStore i d =
  Store i (Uid i d)

type UuidStore d =
  Store UUID (Uuid d)

type family StoreEffects i e ds :: EffectRow where
  StoreEffects _ _ '[] = '[]
  StoreEffects i e (d : ds) = (UidStore i d ! e : StoreEffects i e ds)

type family Stores i e ds r :: Constraint where
  Stores _ _ '[] _ = ()
  Stores i e (d : ds) r = (Member (UidStore i d ! e) r, Stores i e ds r)
