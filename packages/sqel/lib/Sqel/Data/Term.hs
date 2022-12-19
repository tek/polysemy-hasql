module Sqel.Data.Term where

import Sqel.Data.Dd (DdInc (DdMerge, DdNest), DdVar (DdCon, DdProd, DdSum))
import Sqel.Data.PgType (PgColumnName, PgPrimName)
import Sqel.Data.PgTypeName (PgTableName)
import Sqel.Data.Sql (Sql)

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
  Prim PgPrimName Bool [Sql]
  |
  Comp Text Comp CompInc [DdTerm]
  deriving stock (Eq, Show, Generic)

-- TODO would be nice to have a separate type wrapping the root for the table name
data DdTerm =
  DdTerm {
    name :: PgColumnName,
    tableName :: Maybe PgTableName,
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
