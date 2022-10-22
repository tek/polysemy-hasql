module Polysemy.Db.Data.Store where

import Data.UUID (UUID)

import Polysemy.Db.Data.Uid (Uid)

-- |A 'Store' is characterized by a record type @d@ and a primary key type @i@.
-- The parameter is usually something like 'UUID' or 'Int', and it is combined with the record in the data type 'Uid'.
-- Programs using 'Store' need no knowledge about the database that might be backing the effect:
--
-- @
-- data User { name :: Text }
-- progStore :: Member (Store Int User) r => Sem r (Maybe (Uid Int User))
-- progStore = do
--   Store.insert (Uid 1 (User "admin"))
--   Store.upsert (Uid 1 (User "root"))
--   Store.upsert (Uid 2 (User "guest"))
--   _ \<- Store.delete 2
--   Store.fetch 1
-- @
data Store i d :: Effect where
  Insert :: Uid i d -> Store i d m ()
  Upsert :: Uid i d -> Store i d m ()
  Delete :: i -> Store i d m (Maybe (NonEmpty (Uid i d)))
  DeleteAll :: Store i d m (Maybe (NonEmpty (Uid i d)))
  Fetch :: i -> Store i d m (Maybe (Uid i d))
  FetchAll :: Store i d m (Maybe (NonEmpty (Uid i d)))

makeSem ''Store

type UuidStore d =
  Store UUID d

type family StoreEffects i e ds :: EffectRow where
  StoreEffects _ _ '[] = '[]
  StoreEffects i e (d : ds) = (Store i d !! e : StoreEffects i e ds)

type family Stores i e ds r :: Constraint where
  Stores _ _ '[] _ = ()
  Stores i e (d : ds) r = (Member (Store i d !! e) r, Stores i e ds r)
