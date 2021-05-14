module Polysemy.Hasql.Table.Query.SetPartial where

import Polysemy.Hasql.Data.DbType (Column(Column, _selector), Selector (Selector, unSelector))
import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))
import Polysemy.Hasql.DbType (baseColumnSelectors, flatColumns)
import Polysemy.Hasql.Table.Query.Text (commaSeparated)
import Polysemy.Hasql.Table.Query.Prepared (dollar)
import Polysemy.Db.Data.PartialFields (PartialFields)

conditionalColumn :: Column -> Int -> Int -> Text
conditionalColumn Column {_selector = Selector s} c i =
  [text|when case #{dollar i} then #{dollar c} else old.#{s} end|]

conditional :: [Column] -> Int -> [Text]
conditional cols flagStart =
  uncurry (uncurry conditionalColumn) <$> zip (zip cols [1..]) [flagStart..]

setPartial ::
  Column ->
  SqlCode
setPartial column =
  SqlCode [text|set (#{names}) = (#{values})|]
  where
    names =
      commaSeparated (unSelector <$> baseColumnSelectors column)
    values =
      commaSeparated (conditional flat (length flat))
    flat =
      flatColumns column

setPartialDynamic ::
  Column ->
  PartialFields d ->
  SqlCode
setPartialDynamic =
  undefined
