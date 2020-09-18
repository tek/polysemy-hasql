module Polysemy.Hasql.Table.Query.Set where

import Polysemy.Db.Data.TableStructure (Column(Column))
import Polysemy.Db.Data.TableStructure (TableStructure(TableStructure))
import Polysemy.Hasql.Table.Query.Text (commaSeparated)
import Polysemy.Db.Text.Quote (dquote)

set ::
  TableStructure ->
  Text
set (TableStructure _ cols) =
  [qt|set #{values}|]
  where
    values =
      commaSeparated (zipWith assign (toList cols) [(1 :: Int)..])
    assign (Column (dquote -> name) _ _ _) (show @Text -> num) =
      [qt|#{name} = $#{num}|]
