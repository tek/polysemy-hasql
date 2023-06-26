module Polysemy.Hasql.Queue.Store where

import Data.UUID (UUID)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Effect.Store (Store)
import Sqel (Crud, DdType, Def, Json, Prim, PrimAs, Prod, Query, Sqel, UidTable, Uuid, sqel)
import Sqel.Crud (crud)
import Sqel.Exts (Check1, Dd1, ReifySqel)
import Sqel.Migration.Run (RunMigrations)

import Polysemy.Hasql.Effect.Database (Database)
import Polysemy.Hasql.Interpreter.DbTable (interpretTable)
import Polysemy.Hasql.Interpreter.Store (interpretStoreDbCrud)
import Polysemy.Hasql.Queue.Data.Queued (Queued)

type Table_Queued name t d ts = UidTable name UUID (Queued t d) Prim (Prod [ts, Json])

type QueuedCrud :: Symbol -> Type -> Type -> Type -> Dd1 -> Constraint
class QueuedCrud name t d ts table | name t d ts -> table where
  queuedCrud :: (Sqel table, Crud UUID (Uuid (Queued t d)))

instance (
    query ~ Query UUID (PrimAs "id"),
    table ~ Table_Queued name t d ts,
    ReifySqel table,
    Check1 table query
  ) => QueuedCrud name t d ts table where
    queuedCrud =
      (table, crud query table)
      where
        query = sqel @(Query UUID (PrimAs "id"))
        table = sqel @(Table_Queued name t d ts)

interpretQueueStoreDb ::
  ∀ (name :: Symbol) ts d t dt r table .
  DdType table ~ Uuid (Queued t d) =>
  QueuedCrud name t d ts table =>
  RunMigrations Def table =>
  Members [Database !! DbError, Time t dt, Log, Resource, Async, Race, Embed IO, Final IO] r =>
  InterpreterFor (Store UUID (Queued t d) !! DbError) r
interpretQueueStoreDb =
  interpretTable table .
  interpretStoreDbCrud c .
  raiseUnder
  where
    table :: Sqel table
    c :: Crud UUID (Uuid (Queued t d))
    (table, c) = queuedCrud @name @t @d @ts @table
