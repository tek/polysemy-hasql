module Polysemy.Db.Table.Query.Insert where

import Polysemy.Db.Data.Columns (Columns(Columns))
import Polysemy.Db.Data.SqlCode (SqlCode(..))
import Polysemy.Db.Data.TableStructure (TableStructure(TableStructure))
import Polysemy.Db.Table.Query.Fragment (intoFragment)
import Polysemy.Db.Table.Query.Text (commaColumns, commaSeparated)

insert ::
  TableStructure ->
  SqlCode
insert (TableStructure (intoFragment -> SqlCode into) columns@(Columns cs)) =
  SqlCode [i|insert #{into} (#{cols}) values (#{values})|]
  where
    cols =
      commaColumns columns
    values =
      commaSeparated (zipWith dollar [(1 :: Int)..] (toList cs))
    dollar i' _ =
      [i|$#{i'}|]
