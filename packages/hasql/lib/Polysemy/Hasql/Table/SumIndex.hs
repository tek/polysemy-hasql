module Polysemy.Hasql.Table.SumIndex where

import Exon (exon)
import Sqel.SOP.Constraint (DataName)
import Sqel.Text.DbIdentifier (dbDataName)

sumIndexIdentifier ::
  ∀ d name .
  DataName d name =>
  Text
sumIndexIdentifier =
  [exon|ph_sum_index__#{dbDataName @d}|]
