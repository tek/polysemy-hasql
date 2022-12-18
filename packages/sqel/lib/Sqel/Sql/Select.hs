module Sqel.Sql.Select where

import Sqel.Data.QuerySchema (QuerySchema (QuerySchema))
import Sqel.Data.Select (Select (Select))
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.TableSchema (TableSchema (TableSchema))

selectWhere ::
  ∀ proj q table .
  QuerySchema q table ->
  TableSchema proj ->
  Sql
selectWhere (QuerySchema query _) (TableSchema proj _ _) =
  [sql|##{Select proj} ##{query}|]
