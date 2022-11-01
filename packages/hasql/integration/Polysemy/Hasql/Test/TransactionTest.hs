{-# options_ghc -Wno-partial-type-signatures #-}

module Polysemy.Hasql.Test.TransactionTest where

import Exon (exon)
import Lens.Micro.Extras (view)
import qualified Log
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Test (UnitTest, assertJust, assertLeft)
import Sqel.Data.Dd (Dd, DdK (DdK))
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Data.Uid (Uid (Uid))
import Sqel.PgType (tableSchema)
import Sqel.Prim (prim, primAs)
import Sqel.Product (uid)
import Sqel.Query (checkQuery)

import Polysemy.Hasql.Effect.Transaction (Transactions, abort)
import Polysemy.Hasql.Interpreter.Store (interpretManagedTable, interpretStoreDb, interpretStoreXa)
import Polysemy.Hasql.Interpreter.Transaction (interpretTransactions)
import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Hasql.Transaction (transactStores)

data Dat =
  Dat {
    color :: Text
  }
  deriving stock (Eq, Show, Generic)

data Rat =
  Rat {
    color :: Text
  }
  deriving stock (Eq, Show, Generic)

ddDat :: Dd ('DdK _ _ (Uid Int64 Dat) _)
ddDat =
  uid prim prim

tableDat :: TableSchema (Uid Int64 Dat)
tableDat = tableSchema ddDat

queryDat :: QuerySchema Int64 (Uid Int64 Dat)
queryDat =
  checkQuery (primAs @"id") ddDat

tableRat :: TableSchema (Uid Int64 Rat)
queryRat :: QuerySchema Int64 (Uid Int64 Rat)
(tableRat, queryRat) =
  (tableSchema ddRat, checkQuery (primAs @"id") ddRat)
  where
    ddRat = uid prim prim

test_transaction :: UnitTest
test_transaction =
  integrationTest $
  interpretManagedTable tableDat $
  interpretManagedTable tableRat $
  interpretStoreDb tableDat queryDat $
  interpretStoreDb tableRat queryRat $
  interpretStoreXa tableDat queryDat $
  interpretStoreXa tableRat queryRat $
  interpretTransactions $
  do
    let xaError e = Log.error [exon|transaction failed: #{show e}|]
    restop @DbError @(_ _ Dat) (Store.insert (Uid 1 (Dat "gold")))
    restop @DbError @(_ _ Rat) (Store.insert (Uid 1 (Rat "green")))
    resuming @DbError @(Transactions _) xaError $ transactStores @[Dat, Rat] @_ @DbError do
      Store.insert (Uid 2 (Dat "cyan"))
    e <- resumeEither @DbError @(Transactions _) $ transactStores @[Dat, Rat] @_ @DbError do
      Store.insert (Uid 2 (Rat "purple"))
      Store.insert (Uid 3 (Dat "pink"))
      abort
      unit
    resuming @DbError @(Transactions _) xaError $ transactStores @[Dat, Rat] @_ @DbError do
      Store.insert (Uid 3 (Rat "magenta"))
    assertLeft (DbError.Query "aborted by user") e
    assertJust ["gold", "cyan"] . fmap (fmap (view (#payload . #color))) =<< restop @DbError @(_ _ Dat) Store.fetchAll
    assertJust ["green", "magenta"] . fmap (fmap (view (#payload . #color))) =<< restop @DbError @(_ _ Rat) Store.fetchAll
