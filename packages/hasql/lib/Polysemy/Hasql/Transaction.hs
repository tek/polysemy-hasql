module Polysemy.Hasql.Transaction where

import Hasql.Connection (Connection)
import Polysemy.Db.Effect.Store (QStore, Store)
import Sqel (Uid)

import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (ConnectionSource)
import qualified Polysemy.Hasql.Effect.Transaction as Transaction
import Polysemy.Hasql.Effect.Transaction (Transaction, Transactions)
import Polysemy.Internal.CustomErrors (FirstOrder)

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

connectionScope ::
  ∀ eff err r a .
  Member (Scoped ConnectionSource (eff !! err)) r =>
  Connection ->
  (() -> Sem (eff !! err : r) a) ->
  Sem r a
connectionScope conn use =
  scoped (Database.Supplied "transaction" conn) (use ())

interpretForXa ::
  ∀ dep eff err r .
  Member (Scoped ConnectionSource (dep !! err)) r =>
  (∀ x r0 . eff (Sem r0) x -> Sem (Stop err : dep !! err : r) x) ->
  InterpreterFor (Scoped Connection (eff !! err) !! err) r
interpretForXa handler =
  interpretScopedRWith @'[dep !! err] connectionScope \ () -> insertAt @2 . handler

interpretWithXa ::
  ∀ dep eff err r .
  FirstOrder eff "interpretResumable" =>
  Members [Scoped ConnectionSource (dep !! err), dep !! err] r =>
  (∀ x r0 . eff (Sem r0) x -> Sem (Stop err : dep !! err : r) x) ->
  InterpretersFor [Scoped Connection (eff !! err) !! err, eff !! err] r
interpretWithXa handler =
  interpretResumable (subsume_ . raise2Under . handler) .
  interpretForXa (raise2Under . handler)
