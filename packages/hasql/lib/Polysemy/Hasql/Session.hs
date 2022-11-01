module Polysemy.Hasql.Session where

import qualified Data.Text as Text
import Exon (exon)
import Hasql.Connection (Connection)
import qualified Hasql.Session as Session
import Hasql.Session (CommandError (ClientError, ResultError), QueryError (QueryError), Session)
import Hasql.Statement (Statement)
import qualified Polysemy.Db.Data.DbConnectionError as DbConnectionError
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)

convertQueryError :: QueryError -> DbError
convertQueryError (QueryError template (Text.intercalate "," -> args) cmdError) =
  case cmdError of
    ClientError err ->
      DbError.Connection (DbConnectionError.Query (maybe "no error" decodeUtf8 err))
    ResultError err ->
      DbError.Query [exon|#{decodeUtf8 template} #{args} #{show err}|]

runSession ::
  Members [Stop DbError, Embed IO] r =>
  Connection ->
  Session a ->
  Sem r a
runSession connection session = do
  result <- tryIOError (Session.run session connection)
  stopEither (first convertQueryError =<< first DbError.Unexpected result)

runStatement ::
  Members [Stop DbError, Embed IO] r =>
  Connection ->
  p ->
  Statement p a ->
  Sem r a
runStatement connection p stmt =
  runSession connection (Session.statement p stmt)
