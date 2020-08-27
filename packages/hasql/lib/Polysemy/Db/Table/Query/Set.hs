module Polysemy.Db.Table.Query.Set where

import Polysemy.Db.Data.Columns (Column(Column), Columns(Columns))
import Polysemy.Db.Data.TableStructure (TableStructure(TableStructure))
import Polysemy.Db.Table.Query.Text (commaSeparated)
import Polysemy.Db.Text.Quote (dquote)

set ::
  TableStructure ->
  Text
set (TableStructure _ (Columns cols)) =
  [i|set #{values}|]
  where
    values =
      commaSeparated (zipWith assign (toList cols) [(1 :: Int)..])
    assign (Column (dquote -> name) _ _) (show @Text -> num) =
      [i|#{name} = $#{num}|]
