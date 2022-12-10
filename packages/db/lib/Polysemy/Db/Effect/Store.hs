module Polysemy.Db.Effect.Store where

import Data.UUID (UUID)
import Sqel.Data.Uid (Uid)

-- TODO change *All result to [Uid i d]
--
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
