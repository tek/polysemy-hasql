module Polysemy.Db.Data.Store where

import Polysemy.Db.Data.StoreError (StoreError)
import Polysemy.Db.Data.Uid (Uid, Uuid)

data Store p e d :: Effect where
  Insert :: d -> Store p e d m (Either (StoreError e) ())
  Upsert :: d -> Store p e d m (Either (StoreError e) ())
  Delete :: p -> Store p e d m (Either (StoreError e) ())
  Fetch :: p -> Store p e d m (Either (StoreError e) (Maybe d))
  FetchAll :: Store p e d m (Either (StoreError e) (Maybe (NonEmpty d)))

makeSem ''Store

type UidStore i e d =
  Store i e (Uid i d)

type UuidStore e d =
  Store UUID e (Uuid d)

type family StoreEffects i e ds :: EffectRow where
  StoreEffects i e '[] = '[]
  StoreEffects i e (d : ds) = (UidStore i e d : StoreEffects i e ds)

type family Stores i e ds r :: Constraint where
  Stores i e '[] r = ()
  Stores i e (d : ds) r = (Member (UidStore i e d) r, Stores i e ds r)
