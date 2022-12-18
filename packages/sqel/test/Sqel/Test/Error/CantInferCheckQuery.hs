{-# options_ghc -fdefer-type-errors -Wno-deferred-type-errors -Wno-partial-type-signatures #-}

module Sqel.Test.Error.CantInferCheckQuery where

import Sqel.Data.Dd
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Prim (prim)
import Sqel.Product (prod)
import Sqel.Query (CheckQuery (checkQuery))

data Dat =
  Dat {
    name :: Text
  }
  deriving stock (Eq, Show, Generic)

data Q =
  Q {
    name :: Text
  }
  deriving stock (Eq, Show, Generic)

dd :: Dd (_ _ Dat _)
dd =
  undefined

qs :: QuerySchema Q Dat
qs =
  checkQuery (prod prim) dd

cantInferCheckQuery :: Text
cantInferCheckQuery =
  qs
