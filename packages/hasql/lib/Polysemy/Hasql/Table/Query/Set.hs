module Polysemy.Hasql.Table.Query.Set where

import Polysemy.Hasql.Data.DbType (Column)
import Polysemy.Hasql.Data.SqlCode (SqlCode, esql)
import Polysemy.Hasql.DbType (baseColumnSelectors)
import Polysemy.Hasql.Table.Query.Insert (insertValues)
import Polysemy.Hasql.Table.Query.Prepared (assign)
import Polysemy.Hasql.Table.Query.Text (commaSeparated)

set ::
  Column ->
  SqlCode
set column =
  [esql|set #{assigns}|]
  where
    assigns =
      commaSeparated (zipWith assign (baseColumnSelectors column) (insertValues column))
