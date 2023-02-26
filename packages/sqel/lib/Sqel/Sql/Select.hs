module Sqel.Sql.Select where

import Sqel.Data.QuerySchema (QuerySchema (QuerySchema))
import Sqel.Data.Sql (Sql, ToSql, sql)
import Sqel.Data.SqlFragment (Select (Select))
import Sqel.Data.TableSchema (TableSchema (TableSchema))

selectWhere ::
  ∀ proj q table .
  QuerySchema q table ->
  TableSchema proj ->
  Sql
selectWhere (QuerySchema query _) (TableSchema proj _ _) =
  [sql|##{Select proj} ##{query}|]

selectWhereGen ::
  ∀ f proj q table .
  ToSql (Select (f proj table)) =>
  QuerySchema q table ->
  f proj table ->
  Sql
selectWhereGen (QuerySchema query _) proj =
  [sql|##{Select proj} ##{query}|]
