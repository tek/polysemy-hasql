module Polysemy.Hasql.Test.TransactionTest where

import Exon (exon)
import Lens.Micro.Extras (view)
import qualified Log
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Test (Hedgehog, UnitTest, assertEq, assertLeft)
import Sqel.Data.Dd (Dd, DdK (DdK))
import Sqel (QuerySchema)
import Sqel (TableSchema)
import Sqel (Uid (Uid))
import Sqel.PgType (tableSchema)
import Sqel (prim, primAs)
import Sqel (prod)
import Sqel (checkQuery)
import Sqel.Uid (uid)

import Polysemy.Hasql.Effect.Transaction (Transactions, abort)
import Polysemy.Hasql.Interpreter.DbTable (interpretTables)
import Polysemy.Hasql.Interpreter.Store (interpretQStores)
import Polysemy.Hasql.Interpreter.Transaction (interpretTransactions)
import Polysemy.Hasql.Test.RunIntegration (integrationTest)
import Polysemy.Hasql.Transaction (XaStore, transactStores)

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
  uid prim (prod prim)

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
    ddRat = uid prim (prod prim)

prog ::
  Members [XaStore conn DbError Int64 Dat, XaStore conn DbError Int64 Rat] r =>
  Members [Transactions conn !! DbError, Log, Stop DbError, Hedgehog IO] r =>
  Sem r ()
prog = do
  let xaError e = Log.error [exon|transaction failed: #{show e}|]
  resuming @_ @(Transactions _) xaError $ transactStores @['(_, Dat), '(_, Rat)] do
    Store.insert (Uid 2 (Dat "cyan"))
  e <- resumeEither @_ @(Transactions _) $ transactStores @['(_, Dat), '(_, Rat)] do
    Store.insert (Uid 2 (Rat "purple"))
    Store.insert (Uid 3 (Dat "pink"))
    abort
    unit
  resuming @_ @(Transactions _) xaError $ transactStores @['(_, Dat), '(_, Rat)] do
    Store.insert (Uid 3 (Rat "magenta"))
  assertLeft (DbError.Query "aborted by user") e

test_transaction :: UnitTest
test_transaction =
  integrationTest $
  interpretTables tableDat $
  interpretTables tableRat $
  interpretQStores tableDat queryDat $
  interpretQStores tableRat queryRat $
  interpretTransactions do
    restop @DbError @(Store _ Dat) (Store.insert (Uid 1 (Dat "gold")))
    restop @DbError @(Store _ Rat) (Store.insert (Uid 1 (Rat "green")))
    prog
    assertEq ["gold", "cyan"] . fmap (view (#payload . #color)) =<< restop @DbError @(Store _ Dat) Store.fetchAll
    assertEq ["green", "magenta"] . fmap (view (#payload . #color)) =<< restop @DbError @(Store _ Rat) Store.fetchAll
