module Sqel.Data.Term where

import Sqel.Data.ColumnOptions (ColumnOptions)
import Sqel.Data.Dd (DdInc (DdMerge, DdNest), DdVar (DdCon, DdProd, DdSum))
import Sqel.Data.PgType (PgPrimName)

data ProdType =
  Reg
  |
  Con
  deriving stock (Eq, Show, Generic)

data Comp =
  Prod ProdType
  |
  Sum
  deriving stock (Eq, Show, Generic)

data CompInc =
  Merge
  |
  Nest
  deriving stock (Eq, Show, Generic)

data Struct =
  Prim PgPrimName ColumnOptions
  |
  Comp Text Comp CompInc [DdTerm]
  deriving stock (Eq, Show, Generic)

data DdTerm =
  DdTerm {
    name :: Text,
    struct :: Struct
  }
  deriving stock (Eq, Show, Generic)

demoteComp :: DdVar c -> Comp
demoteComp = \case
  DdProd -> Prod Reg
  DdCon -> Prod Con
  DdSum -> Sum

demoteInc :: DdInc i -> CompInc
demoteInc = \case
  DdMerge -> Merge
  DdNest -> Nest
