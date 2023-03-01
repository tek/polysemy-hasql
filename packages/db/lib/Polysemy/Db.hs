module Polysemy.Db (
  -- $intro

  -- * Effects
  module Polysemy.Db.Effect.Store,
  module Polysemy.Db.Effect.Query,
  module Polysemy.Db.Effect.Random,
  module Polysemy.Db.Effect.Id,

  -- * Interpreters
  interpretStoreConc,
  interpretStoreLocal,
  interpretStoreAtomicState,
  interpretStoreState,
  interpretStoreNull,
  PureStore (PureStore),
  pureStore,

  interpretQueryConc,
  interpretQueryConst,
  interpretQueryAtomicState,
  interpretQueryStoreConc,
  interpretQueryStoreAny,

  interpretRandom,
  interpretRandomAtomic,
  interpretRandomState,
  interpretRandomAtomicState,

  interpretIdUuid,
  interpretIdUuidIO,
  interpretIdAtomicState,
  interpretIdNum,
  interpretIdNumLocal,
  interpretIdList,
  interpretIdConst,
  interpretIdUuidZero,

  interpretAtomicStateStore,
  interpretAtomicStateStoreAs,
  interpretAtomicStateStoreScoped,
  interpretAtomicStateStoreScopedAs,
  interpretAtomicStatesStore,

  interpretReaderStore,
  interpretReaderStoreAs,

  -- * Config
  module Polysemy.Db.Data.DbConfig,
  module Polysemy.Db.Data.DbHost,
  module Polysemy.Db.Data.DbPort,
  module Polysemy.Db.Data.DbName,
  module Polysemy.Db.Data.DbUser,
  module Polysemy.Db.Data.DbPassword,

  -- * Errors
  module Polysemy.Db.Data.DbError,
  module Polysemy.Db.Data.InitDbError,
  module Polysemy.Db.Data.DbConnectionError,
) where

import Polysemy.Db.Data.DbConfig (DbConfig (DbConfig))
import Polysemy.Db.Data.DbConnectionError (DbConnectionError)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.DbHost (DbHost (..))
import Polysemy.Db.Data.DbName (DbName (..))
import Polysemy.Db.Data.DbPassword (DbPassword (..))
import Polysemy.Db.Data.DbPort (DbPort (..))
import Polysemy.Db.Data.DbUser (DbUser (..))
import Polysemy.Db.Data.InitDbError (InitDbError (..))
import Polysemy.Db.Effect.Id (Id, newId)
import Polysemy.Db.Effect.Query (Query, query)
import Polysemy.Db.Effect.Random (Random, random, randomR)
import Polysemy.Db.Effect.Store (
  QStore,
  Store,
  alter,
  delete,
  deleteAll,
  elem,
  fetch,
  fetchAll,
  fetchPayload,
  insert,
  upsert,
  )
import Polysemy.Db.Interpreter.AtomicState (
  interpretAtomicStateStore,
  interpretAtomicStateStoreAs,
  interpretAtomicStateStoreScoped,
  interpretAtomicStateStoreScopedAs,
  interpretAtomicStatesStore,
  )
import Polysemy.Db.Interpreter.Id (
  interpretIdAtomicState,
  interpretIdConst,
  interpretIdList,
  interpretIdNum,
  interpretIdNumLocal,
  interpretIdUuid,
  interpretIdUuidIO,
  interpretIdUuidZero,
  )
import Polysemy.Db.Interpreter.Query (
  interpretQueryAtomicState,
  interpretQueryConc,
  interpretQueryConst,
  interpretQueryStoreAny,
  interpretQueryStoreConc,
  )
import Polysemy.Db.Interpreter.Random (
  interpretRandom,
  interpretRandomAtomic,
  interpretRandomAtomicState,
  interpretRandomState,
  )
import Polysemy.Db.Interpreter.Reader (interpretReaderStore, interpretReaderStoreAs)
import Polysemy.Db.Interpreter.Store (
  PureStore (PureStore),
  interpretStoreAtomicState,
  interpretStoreConc,
  interpretStoreLocal,
  interpretStoreNull,
  interpretStoreState,
  pureStore,
  )

-- $intro
-- The 'Polysemy' effects 'Store' and 'Query' provide a high-level abstraction of database operations for
-- CRUD and arbitrary queries.
