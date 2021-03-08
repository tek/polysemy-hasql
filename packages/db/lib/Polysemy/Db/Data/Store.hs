module Polysemy.Db.Data.Store where

import Polysemy.Db.Data.Uid (Uid, Uuid)

-- |A 'Store' is characterized by a record type @d@ and a parameter type @p@.
-- The parameter is usually something like 'UUID' or 'Int', assumed to be a field in @d@.
-- Programs using 'Store' need no knowledge about the database that might be backing the effect:
--
-- @
-- data User { id :: Int, name :: Text }
-- progStore :: Member (Store Int User) r => Sem r (Maybe User)
-- progStore = do
--   Store.insert (User 1 "admin")
--   Store.upsert (User 1 "root")
--   Store.upsert (User 2 "guest")
--   _ \<- Store.delete 2
--   Store.fetch 1
-- @
--
-- The parameter type doesn't necessarily have to correspond to the field's type; it's up to the interpreter to make
-- them agree.
data Store p d :: Effect where
  Insert :: d -> Store p d m ()
  Upsert :: d -> Store p d m ()
  Delete :: p -> Store p d m (Maybe (NonEmpty d))
  DeleteAll :: Store p d m (Maybe (NonEmpty d))
  Fetch :: p -> Store p d m (Maybe d)
  FetchAll :: Store p d m (Maybe (NonEmpty d))

makeSem ''Store

type UidStore i d =
  Store i (Uid i d)

type UuidStore d =
  Store UUID (Uuid d)

type family StoreEffects i e ds :: EffectRow where
  StoreEffects _ _ '[] = '[]
  StoreEffects i e (d : ds) = (UidStore i d !! e : StoreEffects i e ds)

type family Stores i e ds r :: Constraint where
  Stores _ _ '[] _ = ()
  Stores i e (d : ds) r = (Member (UidStore i d !! e) r, Stores i e ds r)
