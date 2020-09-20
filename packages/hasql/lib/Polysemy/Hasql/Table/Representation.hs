module Polysemy.Hasql.Table.Representation where

import Data.Vector (Vector)
import Generics.SOP (Code)
import Prelude hiding (Enum)
import Type.Errors (ErrorMessage(Text, ShowType), TypeError)
import Type.Errors.Pretty (type (<>))

import Polysemy.Db.Data.Column (Auto, Enum, Flatten, Prim, Sum)
import Polysemy.Db.SOP.Error (ErrorWithType, JoinComma)
import Polysemy.Db.SOP.FieldNames (FieldNames)

data PrimColumn =
  PrimColumn
  deriving (Show)

data EnumColumn =
  EnumColumn
  deriving (Show)

data ProdColumn (ds :: [*]) =
  ProdColumn
  deriving (Show)

data SumColumn (dss :: [[*]]) =
  SumColumn
  deriving (Show)

data ProdTable (ds :: [*]) =
  ProdTable
  deriving (Show)

data SumTable (dss :: [[*]]) =
  SumTable
  deriving (Show)

type family PrimColumnCode d :: * where
  PrimColumnCode d = PrimColumn

type family CtorColumnCode (ds :: [*]) :: [*] where
  CtorColumnCode '[] = '[]
  CtorColumnCode (d : ds) = ColumnCode d : CtorColumnCode ds

type family CtorsColumnCode (dss :: [[*]]) :: [[*]] where
  CtorsColumnCode '[] = '[]
  CtorsColumnCode (ds : dss) = CtorColumnCode ds : CtorsColumnCode dss

type family DataColumnCode (dss :: [[*]]) (initial :: [[*]]) :: * where
  DataColumnCode '[d1 : ds] '[d1 : ds] = ProdColumn (ColumnCodes (d1 : ds))
  DataColumnCode ('[] : dss) initial = DataColumnCode dss initial
  DataColumnCode '[] initial = EnumColumn
  DataColumnCode ((d1 : ds) : dss) initial = SumColumn (CtorsColumnCode initial)
  DataColumnCode dss initial =
    TypeError ('Text "could not match sum type column: " <> 'ShowType initial)

type family ColumnCode (d :: *) :: * where
  ColumnCode Int = PrimColumnCode Int
  ColumnCode Text = PrimColumnCode Text
  ColumnCode ByteString = PrimColumnCode ByteString
  ColumnCode String = PrimColumnCode String
  ColumnCode UUID = PrimColumnCode UUID
  ColumnCode Float = PrimColumnCode Float
  ColumnCode Double = PrimColumnCode Double
  ColumnCode [d] = PrimColumnCode [d]
  ColumnCode (NonEmpty d) = PrimColumnCode (NonEmpty d)
  ColumnCode (Vector d) = PrimColumnCode (Vector d)
  ColumnCode d = DataColumnCode (Code d) (Code d)

type family ColumnCodes (ds :: [*]) :: [*] where
  ColumnCodes '[] = '[]
  ColumnCodes (d : ds) = ColumnCode d : ColumnCodes ds

type family ColumnCodess (ds :: [[*]]) :: [[*]] where
  ColumnCodess '[] = '[]
  ColumnCodess (d : ds) = ColumnCodes d : ColumnCodess ds

type family ColumnRep (d :: *) :: * where
  ColumnRep PrimColumn = Prim Auto
  ColumnRep (SumColumn dss) = Sum (SumColumn (ColumnRepss dss))
  ColumnRep (ProdColumn ds) = Flatten (ProdColumn (ColumnReps ds))
  ColumnRep EnumColumn = Enum Auto

type family ColumnReps (d :: [*]) :: [*] where
  ColumnReps '[] = '[]
  ColumnReps (d : ds) = ColumnRep d : ColumnReps ds

type family ColumnRepss (d :: [[*]]) :: [[*]] where
  ColumnRepss '[] = '[]
  ColumnRepss (ds : dss) = ColumnReps ds : ColumnRepss dss

type family TableRep (d :: [[*]]) :: * where
  TableRep '[ds] = ProdTable ds
  TableRep dss = SumColumn dss

type family ProdCode (d :: [[*]]) :: [*] where
  ProdCode '[ds] = ds
  ProdCode _ = TypeError ('Text "not a product type")

type family Rep d :: * where
  Rep d = TableRep (ColumnRepss (ColumnCodess (Code d)))

type family ExplicitColumn (dt :: *) (rep :: *) (rn :: Symbol) (d :: *) (dn :: Symbol) :: * where
  ExplicitColumn dt (Sum rep) rn d dn =
    Sum (SumColumn (ExplicitSum d rep))
  ExplicitColumn dt (Flatten rep) rn d dn =
    Flatten (ProdColumn (ProdCode (ExplicitSum d rep)))
  ExplicitColumn dt (Enum rep) rn d dn =
    Enum rep
  ExplicitColumn dt rep rn d dn =
    rep

type family FieldMismatch (dt :: *) (rs :: [*]) (rns :: [Symbol]) (ds :: [*]) (dns :: [Symbol]) :: k where
  FieldMismatch dt reps rns '[] _ =
    TypeError ('Text "too many fields in rep for " <> 'ShowType dt <> ": " <> JoinComma rns)
  FieldMismatch dt '[] _ ds dns =
    TypeError ('Text "missing fields in rep for " <> 'ShowType dt <> ": " <> JoinComma dns)
  FieldMismatch dt _ _ _ _ =
    ErrorWithType "unexpected field mismatch when checking table representation" dt

type family ExplicitProd (dt :: *) (rs :: [*]) (rns :: [Symbol]) (ds :: [*]) (dns :: [Symbol]) :: [*] where
  ExplicitProd dt '[] _ '[] _ = '[]
  ExplicitProd dt (rep : reps) (rn : rns) (d : ds) (dn : dns) =
    ExplicitColumn dt rep rn d dn : ExplicitProd dt reps rns ds dns
  ExplicitProd dt rs rns ds dns =
    FieldMismatch dt rs rns ds dns

type family CtorMismatch dt :: k where
  CtorMismatch dt =
    ErrorWithType "constructor mismatch for database representation" dt

type Names = [[Symbol]]

type family GenExplicitSum (dt :: *) (repss :: [[*]]) (rns :: Names) (dss :: [[*]]) (dns :: Names) :: [[*]] where
  GenExplicitSum dt '[] _ '[] _ = '[]
  GenExplicitSum dt '[] _ dss _ = CtorMismatch dt
  GenExplicitSum dt repss _ '[] _ = CtorMismatch dt
  GenExplicitSum dt (rs : rss) (rn : rns) (ds : dss) (dn : dns) =
    ExplicitProd dt rs rn ds dn : GenExplicitSum dt rss rns dss dns
  GenExplicitSum dt _ _ _ _ = CtorMismatch dt

type family ExplicitSum (dt :: *) (rep :: *) :: [[*]] where
  ExplicitSum dt rep =
    GenExplicitSum dt (Code rep) (FieldNames rep) (Code dt) (FieldNames dt)

type family Explicit (rep :: *) (dt :: *) :: * where
  Explicit Auto dt = Rep dt
  Explicit rep dt = TableRep (ExplicitSum dt rep)

type family SumAsTable d :: k where
  SumAsTable d =
    ErrorWithType "cannot use sum type for a table" d

type family ReifyRepTable (rep :: *) (d :: *) :: [*] where
  ReifyRepTable (SumTable _) d = SumAsTable d
  ReifyRepTable (SumColumn _) d = SumAsTable d
  ReifyRepTable (ProdTable rep) _ = rep
  ReifyRepTable (ProdColumn rep) _ = rep
  ReifyRepTable rep d = ReifyRepTable (Explicit rep d) d

type family ProductForSum d :: k where
  ProductForSum d =
    ErrorWithType "cannot use product type for a sum column" d

type family ReifySumType (rep :: *) (d :: *) :: [[*]] where
  ReifySumType (SumTable _) d = SumAsTable d
  ReifySumType (SumColumn rep) d = rep
  ReifySumType (ProdTable _) d = ProductForSum d
  ReifySumType (ProdColumn _) d = ProductForSum d
  ReifySumType rep d = ExplicitSum d rep
