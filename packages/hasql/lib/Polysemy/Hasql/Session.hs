module Polysemy.Hasql.Session where

import qualified Data.Text as Text
import Hasql.Connection (Connection)
import Hasql.Session (CommandError(ClientError, ResultError), QueryError(QueryError), Session)
import qualified Hasql.Session as Session (run)

import qualified Polysemy.Db.Data.DbConnectionError as DbConnectionError
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)

convertQueryError :: QueryError -> DbError
convertQueryError (QueryError template (Text.intercalate "," -> args) cmdError) =
  case cmdError of
    ClientError err ->
      DbError.Connection (DbConnectionError.Query (maybe "no error" decodeUtf8 err))
    ResultError err ->
      DbError.Query [qt|#{template} #{args} #{err}|]

runSession ::
  Member (Embed IO) r =>
  Connection ->
  Session a ->
  Sem r (Either DbError a)
runSession connection session =
  mapLeft convertQueryError <$> embed (Session.run session connection)
