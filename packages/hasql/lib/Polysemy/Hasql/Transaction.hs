module Polysemy.Hasql.Transaction where

import Polysemy.Db.Effect.Store (QStore, Store)
import Sqel.Data.Uid (Uid)

import qualified Polysemy.Hasql.Effect.Transaction as Transaction
import Polysemy.Hasql.Effect.Transaction (Transaction, Transactions)

type XaQStore res err f q d =
  Scoped res (QStore f q d !! err) !! err

type XaStore res err i d =
  XaQStore res err Maybe i (Uid i d)

type TransactionEffects :: EffectRow -> EffectRow -> Type -> Type -> EffectRow -> Constraint
class TransactionEffects all effs err res r where
  transactionEffects :: res -> InterpretersFor effs r

instance TransactionEffects all '[] err res r where
  transactionEffects _ = id

instance (
    TransactionEffects all effs err res r,
    Member (Scoped res (eff !! err) !! err) (effs ++ r),
    Member (Stop err) (effs ++ r)
  ) => TransactionEffects all (eff : effs) err res r where
  transactionEffects res =
    transactionEffects @all @effs @err res .
    restop @err @(Scoped res (eff !! err)) .
    scoped res .
    restop @err @eff .
    raiseUnder2

-- TODO add scope parameter of type TransactionConfig
transact ::
  ∀ effs res err r .
  effs ++ '[Transaction res] ++ r ~ (effs ++ '[Transaction res]) ++ r =>
  Member (Transactions res) r =>
  TransactionEffects effs effs err res (Transaction res : r) =>
  InterpretersFor (effs ++ '[Transaction res]) r
transact ma =
  scoped_ do
    conn <- Transaction.resource
    transactionEffects @effs @effs @err conn do
      ma

type family XaStores (ds :: [(Type, Type)]) :: [Effect] where
  XaStores '[] = '[]
  XaStores ('(i, d) : ds) = Store i d : XaStores ds

transactStores ::
  ∀ ds res err r xas .
  xas ~ XaStores ds =>
  XaStores ds ++ (Transaction res : r) ~ (xas ++ '[Transaction res]) ++ r =>
  Member (Transactions res) r =>
  TransactionEffects xas xas err res (Transaction res : r) =>
  InterpretersFor (xas ++ '[Transaction res]) r
transactStores ma =
  scoped_ do
    conn <- Transaction.resource
    transactionEffects @xas @xas @err conn do
      ma
