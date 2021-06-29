module Polysemy.Hasql.Table.SumIndex where

import Polysemy.Db.SOP.Constraint (DataName)
import Polysemy.Db.Text.DbIdentifier (dbDataName)

sumIndexIdentifier ::
  ∀ d name .
  DataName d name =>
  Text
sumIndexIdentifier =
  [text|ph_sum_index__#{dbDataName @d}|]
