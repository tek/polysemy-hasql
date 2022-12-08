module Polysemy.Hasql.Effect.Database where

import Hasql.Connection (Connection)
import qualified Hasql.Session as Session
import Hasql.Session (Session)
import Hasql.Statement (Statement)
import Polysemy.Db.Data.DbError (DbError)
import Prelude hiding (tag)

import Polysemy.Hasql.Data.ConnectionTag (ConnectionTag)
import Polysemy.Hasql.Data.InitDb (InitDb)

data ConnectionSource =
  Global
  |
  Unique (Maybe ConnectionTag)
  |
  Supplied ConnectionTag Connection

data Database :: Effect where
  Tag :: Database m ConnectionTag
  Release :: Database m ()
  Retry :: TimeUnit t => t -> Maybe Int -> m a -> Database m a
  WithInit :: InitDb m -> m a -> Database m a
  Use :: (Connection -> m a) -> Database m a
  Session :: Session a -> Database m a
  ResetInit :: Database m ()

makeSem ''Database

type Databases =
  Scoped ConnectionSource (Database !! DbError)

withDatabaseUnique ::
  Member Databases r =>
  Maybe ConnectionTag ->
  InterpreterFor (Database !! DbError) r
withDatabaseUnique t =
  scoped (Unique t)

withDatabaseGlobal ::
  Member Databases r =>
  InterpreterFor (Database !! DbError) r
withDatabaseGlobal =
  scoped Global

statement ::
  Member Database r =>
  p ->
  Statement p o ->
  Sem r o
statement p s =
  session (Session.statement p s)
