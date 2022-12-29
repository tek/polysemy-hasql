module Polysemy.Hasql.Queue.Store where

import Data.UUID (UUID)
import Generics.SOP (NP (Nil, (:*)))
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Effect.Store (Store)
import Prelude hiding (Queue, listen)
import Sqel.Codec (PrimColumn)
import Sqel.Data.Dd (DbTypeName)
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Data.Uid (Uuid)
import Sqel.PgType (tableSchema)
import qualified Sqel.Prim as Sqel
import Sqel.Prim (prim, primAs)
import Sqel.Product2 (prod)
import Sqel.Query (checkQuery)
import Sqel.Uid (uid)

import Polysemy.Hasql.Effect.Database (Database)
import Polysemy.Hasql.Interpreter.DbTable (interpretDbTable)
import Polysemy.Hasql.Interpreter.Store (interpretStoreDb)
import Polysemy.Hasql.Queue.Data.Queued (Queued)

class StoreTable t d where
  storeTable :: (TableSchema (Uuid (Queued t d)), QuerySchema UUID (Uuid (Queued t d)))

instance (
    ToJSON d,
    FromJSON d,
    PrimColumn t,
    DbTypeName d name,
    KnownSymbol (AppendSymbol "Queued" name)
  ) => StoreTable t d where
  storeTable =
    (ts, qs)
    where
      ts = tableSchema table
      qs = checkQuery query table
      query = primAs @"id"
      table = uid prim (prod (prim :* Sqel.json :* Nil))

interpretQueueStoreDb ::
  ∀ d t dt r .
  StoreTable t d =>
  Members [Database !! DbError, Time t dt, Log, Resource, Async, Race, Embed IO, Final IO] r =>
  InterpreterFor (Store UUID (Queued t d) !! DbError) r
interpretQueueStoreDb =
  interpretDbTable ts .
  interpretStoreDb ts qs .
  raiseUnder
  where
    (ts, qs) = storeTable @t
