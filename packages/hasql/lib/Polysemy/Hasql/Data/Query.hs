module Polysemy.Hasql.Data.Query where

import Hasql.Encoders (Params)
import Polysemy.Hasql.Data.Where (Where)

data Query q d :: Effect where
  Params :: Query q d m (Params q)
  Query :: Query q d m (Where q d)

makeSem ''Query
