{-# options_ghc -Wno-partial-type-signatures -fdefer-type-errors -Wno-deferred-type-errors #-}

module Sqel.Test.Error.CompArgs where

import Generics.SOP (NP (Nil, (:*)))

import Sqel.Column (nullable)
import Sqel.Data.Dd (Dd (Dd), DdK (DdK), DdStruct (DdComp), (:>) ((:>)))
import Sqel.Data.PgType (PgTable)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.PgType (tableSchema)
import Sqel.Prim (prim)
import Sqel.Product (prod)

data Pr =
  Pr {
    pr1 :: Text,
    pr2 :: Int,
    pr3 :: Text
  }
  deriving stock (Eq, Show, Generic)

data Dat =
  Dat {
    name :: Maybe Text,
    p :: Pr
  }
  deriving stock (Eq, Show, Generic)

ddTooFew :: Dd ('DdK _ _ Dat _)
ddTooFew =
  prod (
    nullable prim :>
    prod prim
  )

tableTooFew :: TableSchema Dat
tableTooFew =
  tableSchema ddTooFew

prodTooFew :: PgTable Dat
prodTooFew =
  tableTooFew ^. #pg

ddTooMany :: Dd ('DdK _ _ Dat _)
ddTooMany =
  prod (
    nullable prim :>
    prod (prim :> prim :> prim :> prim :> prim)
  )

tableTooMany :: TableSchema Dat
tableTooMany =
  tableSchema ddTooMany

prodTooMany :: PgTable Dat
prodTooMany =
  tableTooMany ^. #pg

ddBadType :: Dd ('DdK _ _ Dat _)
ddBadType =
  prod (
    nullable prim :>
    prod (prim :> False :> prim)
  )

prodBadType :: ()
prodBadType =
  case ddBadType of
    Dd _ _ (DdComp _ _ _ (_ :* (Dd _ _ (DdComp _ _ _ (_ :* (Dd _ _ _) :* _ :* Nil))) :* Nil)) ->
      ()
