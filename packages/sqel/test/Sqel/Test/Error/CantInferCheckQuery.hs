{-# options_ghc -fdefer-type-errors -Wno-deferred-type-errors -Wno-partial-type-signatures #-}

module Sqel.Test.Error.CantInferCheckQuery where

import Sqel.Data.Dd
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.Sql (Sql, ToSql (toSql))
import Sqel.Prim (prim)
import Sqel.Product (prod)
import Sqel.Query (CheckQuery (checkQuery))
import Sqel.Sql.SelectQuery (SelectQuery (SelectQuery))

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

ddq :: Dd (_ _ Q _)
ddq =
  prod prim

qs :: QuerySchema Q Dat
qs =
  checkQuery ddq dd

cantInferCheckQuery :: Sql
cantInferCheckQuery =
  toSql (SelectQuery qs)
