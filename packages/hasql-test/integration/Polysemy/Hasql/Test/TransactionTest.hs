module Polysemy.Hasql.Test.TransactionTest where

import Exon (exon)
import Lens.Micro.Extras (view)
import qualified Log
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Test (Hedgehog, UnitTest, assertEq, assertLeft)
import Sqel (Gen, Prim, Sqel, Uid (Uid), UidTable, query_Int, sqel)

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

table_Dat :: Sqel (UidTable "dat" Int64 Dat Prim Gen)
table_Dat = sqel

data Rat =
  Rat {
    color :: Text
  }
  deriving stock (Eq, Show, Generic)

table_Rat :: Sqel (UidTable "rat" Int64 Rat Prim Gen)
table_Rat = sqel

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
  interpretTables table_Dat $
  interpretTables table_Rat $
  interpretQStores query_Int table_Dat $
  interpretQStores query_Int table_Rat $
  interpretTransactions do
    restop @DbError @(Store _ Dat) (Store.insert (Uid 1 (Dat "gold")))
    restop @DbError @(Store _ Rat) (Store.insert (Uid 1 (Rat "green")))
    prog
    assertEq ["gold", "cyan"] . fmap (view (#payload . #color)) =<< restop @DbError @(Store _ Dat) Store.fetchAll
    assertEq ["green", "magenta"] . fmap (view (#payload . #color)) =<< restop @DbError @(Store _ Rat) Store.fetchAll
