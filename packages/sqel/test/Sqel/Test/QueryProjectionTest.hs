{-# options_ghc -Wno-partial-type-signatures #-}

module Sqel.Test.QueryProjectionTest where

import Hedgehog (TestT, (===))
import Prelude hiding (sum)

import Sqel.Class.MatchView (HasPath, HasField)
import Sqel.Column (pk)
import Sqel.Data.Dd (Dd, DdType, Sqel, type (:>) ((:>)))
import Sqel.Data.Order (Order (Desc))
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Uid (Uid)
import Sqel.Merge (merge)
import Sqel.Names (named)
import Sqel.PgType (CheckedProjection, MkTableSchema, projection)
import Sqel.Prim (prim, prims)
import Sqel.Product (prod)
import Sqel.Query (checkQuery)
import qualified Sqel.Query.Combinators as Q
import Sqel.Sql.Select (selectWhereGen)
import Sqel.Sum (con, con1, sum)
import Sqel.Type (MSelect, Merge, Name, Prim, PrimUnused, Prod, type (*>), type (>))
import Sqel.Uid (uid)

data Thing =
  Thing {
    t_a :: Int,
    t_b :: Text,
    t_c :: Double
  }
  deriving stock (Eq, Show, Generic)

type DdThing =
  Prod Thing *>
    Prim "t_a" Int >
    Prim "t_b" Text >
    Prim "t_c" Double

ddThing :: Dd DdThing
ddThing =
  prod prims

data Something =
  Something {
    st_a :: Int,
    thing :: Thing
  }
  deriving stock (Eq, Show, Generic)

ddSomething :: Sqel (Uid Int64 Something) _
ddSomething =
  uid (pk prim) (prod (prim :> merge ddThing))

data SomethingNest =
  SomeThingNest { thing :: Thing }
  |
  NotAThingNest { nat_a :: Int, nat_b :: Text }
  deriving stock (Eq, Show, Generic)

ddSomethingNest :: Sqel (Uid Int64 SomethingNest) _
ddSomethingNest =
  uid (pk prim) (sum (con1 ddThing :> con prims))

data QThing (a :: Type) =
  QThing {
    t_b :: Text,
    t_c :: Double
  }
  deriving stock (Eq, Show, Generic)

-- TODO think of something better for @order@
data QThingI d =
  QThingI {
    qt :: QThing d,
    limit :: Int64,
    order :: ()
  }
  deriving stock (Eq, Show, Generic)

type QThingDd d =
  Prod (QThing d) *>
    Prim "t_b" Text >
    Prim "t_c" Double

data ThingWrap a =
  ThingWrap { thing :: a }
  deriving stock (Eq, Show, Generic)

type DdThingWrap d =
  Prod (ThingWrap (DdType d)) *> Name "thing" d

data ThingFor d =
  ThingFor { thing :: Thing }
  deriving stock (Eq, Show, Generic)

type DdThingForNest d =
  Prod (ThingFor d) *> Name "thing" DdThing

ddThingNest :: Dd (DdThingForNest d)
ddThingNest =
  prod ddThing

-- TODO also here, the order field must be renamed to @thing@ because that's what it orders on
type DdQThingI d =
  Prod (QThingI d) *>
    Merge (
      Prod (QThing d) *>
        Prim "t_b" Text >
        Prim "t_c" Double
    ) >
    MSelect (PrimUnused Int64) >
    MSelect (Prim "t_c" ())

type DdQThingINest d =
  Prod (ThingWrap (QThingI d)) *>
  Name "thing" (DdQThingI d)

ddDdQThingI :: Dd (DdQThingI d)
ddDdQThingI =
  prod (merge (prod prims) :> Q.limit :> named @"t_c" (Q.order Desc))

qthingSql ::
  ∀ s .
  HasField "t_b" Text s =>
  HasField "t_c" Double s =>
  HasField "t_c" () s =>
  CheckedProjection DdThing s =>
  MkTableSchema s =>
  Dd s ->
  Sql
qthingSql table =
  selectWhereGen qs ps
  where
    qs = checkQuery query table
    ps = projection ddThing table
    query = ddDdQThingI

qthingNestSql ::
  ∀ s .
  HasPath ["thing", "t_b"] Text s =>
  HasPath ["thing", "t_c"] Double s =>
  HasPath ["thing", "t_c"] () s =>
  MkTableSchema s =>
  CheckedProjection (DdThingForNest (DdType s)) s =>
  Dd s ->
  Sql
qthingNestSql table =
  selectWhereGen qs ps
  where
    qs = checkQuery query table
    ps = projection (ddThingNest @(DdType s)) table
    query = prod @(ThingWrap _) ddDdQThingI

targetThing :: Sql
targetThing =
  [sql|select "t_a", "t_b", "t_c" from "something" where (("t_b" = $1 and "t_c" = $2)) order by "t_c" desc limit $3|]

targetThingNest :: Sql
targetThingNest =
  [sql|select ("thing").t_a, ("thing").t_b, ("thing").t_c from "something_nest" where (((("thing")."t_b" = $1 and ("thing")."t_c" = $2))) order by ("thing")."t_c" desc limit $3|]

test_queryProjection :: TestT IO ()
test_queryProjection = do
  targetThing === qthingSql ddSomething
  targetThingNest === qthingNestSql ddSomethingNest
