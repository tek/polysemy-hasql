module Polysemy.Hasql.Table.Query.Set where

import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))
import Polysemy.Hasql.Table.Query.Insert (insertValues)
import Polysemy.Hasql.Table.Query.Prepared (assign)
import Polysemy.Hasql.Table.Query.Text (commaSeparated)
import Polysemy.Hasql.Data.DbType (Column)
import Polysemy.Hasql.DbType (baseColumnSelectors)

set ::
  Column ->
  SqlCode
set column =
  SqlCode [qt|set #{assigns}|]
  where
    assigns =
      commaSeparated (zipWith assign (baseColumnSelectors column) (insertValues column))
