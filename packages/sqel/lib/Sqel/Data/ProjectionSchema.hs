module Sqel.Data.ProjectionSchema where

import Hasql.Decoders (Row)

import Sqel.Data.Selector (Selector)

data ProjectionSchema a =
  ProjectionSchema {
    column :: [Selector],
    decoder :: Row a
  }
  deriving stock (Generic)
