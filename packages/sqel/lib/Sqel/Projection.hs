module Sqel.Projection where

import Sqel.Data.Dd (DdK)

type Projection :: DdK -> DdK -> Constraint
class Projection table view where
