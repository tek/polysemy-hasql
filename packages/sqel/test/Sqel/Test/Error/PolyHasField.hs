{-# options_ghc -fdefer-type-errors -Wno-deferred-type-errors #-}

module Sqel.Test.Error.PolyHasField where

import Prelude hiding (sum)

import Sqel.Class.MatchView (HasField)
import Sqel.Data.Dd (Dd)
import Sqel.Data.Sql (Sql)
import Sqel.PgType (CheckedProjection, MkTableSchema, projection)
import Sqel.Prim (prims)
import Sqel.Product (prod)
import Sqel.Query (checkQuery)
import Sqel.Sql.Select (selectWhereGen)
import Sqel.Type (ProdPrims)

data Q =
  Q {
    f1 :: Text,
    f2 :: Int
  }
  deriving stock (Eq, Show, Generic)

ddQ :: Dd (ProdPrims Q)
ddQ =
  prod prims

data Dat =
  Dat {
    f1 :: Text,
    f2 :: Int,
    f3 :: Double
  }
  deriving stock (Eq, Show, Generic)

ddDat :: Dd (ProdPrims Dat)
ddDat =
  prod prims

mkSql ::
  ∀ s .
  HasField "f1" Text s =>
  CheckedProjection s s =>
  MkTableSchema s =>
  Dd s ->
  Sql
mkSql table =
  selectWhereGen qs ps
  where
    qs = checkQuery query table
    ps = projection table table
    query = ddQ

polyHasField :: Sql
polyHasField = mkSql ddDat
