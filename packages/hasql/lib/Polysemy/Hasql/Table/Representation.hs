module Polysemy.Hasql.Table.Representation where

import Data.Vector (Vector)
import Generics.SOP.GGP (GCode)
import Prelude hiding (Enum)
import Type.Errors (ErrorMessage(Text, ShowType), TypeError)
import Type.Errors.Pretty (type (<>))

import Polysemy.Db.Data.Column (Auto, Enum, Flatten, Prim, Sum)
import Polysemy.Db.SOP.Error (ErrorWithType, JoinComma, MessageWithType)
import Polysemy.Db.SOP.FieldNames (FieldNames)

data ProdColumn (ds :: [*]) =
  ProdColumn
  deriving (Show)

data SumColumn (dss :: [*]) =
  SumColumn
  deriving (Show)

data ProdTable (ds :: [*]) =
  ProdTable
  deriving (Show)

data SumTable (dss :: [[*]]) =
  SumTable
  deriving (Show)

type family SumAsTable d :: k where
  SumAsTable d =
    ErrorWithType "cannot use sum type for a table" d

type family CtorColumnCode (ds :: [*]) :: [*] where
  CtorColumnCode '[] = '[]
  CtorColumnCode (d : ds) = ColumnCode d : CtorColumnCode ds

type family CtorsColumnCode (dss :: [[*]]) :: [*] where
  CtorsColumnCode '[] = '[]
  CtorsColumnCode (ds : dss) = ProdColumn (CtorColumnCode ds) : CtorsColumnCode dss

type family DataColumnCode (dss :: [[*]]) (initial :: [[*]]) :: * where
  DataColumnCode '[d1 : ds] '[d1 : ds] = Flatten (ProdColumn (ColumnCodes (d1 : ds)))
  DataColumnCode ('[] : dss) initial = DataColumnCode dss initial
  DataColumnCode '[] initial = Enum Auto
  DataColumnCode ((d1 : ds) : dss) initial = Sum (SumColumn (CtorsColumnCode initial))
  DataColumnCode dss initial =
    TypeError ('Text "could not match sum type column: " <> 'ShowType initial)

type family ColumnCode (d :: *) :: * where
  ColumnCode Int = Prim Auto
  ColumnCode Text = Prim Auto
  ColumnCode ByteString = Prim Auto
  ColumnCode String = Prim Auto
  ColumnCode UUID = Prim Auto
  ColumnCode Float = Prim Auto
  ColumnCode Double = Prim Auto
  ColumnCode (Maybe d) = Prim Auto
  ColumnCode [d] = Prim Auto
  ColumnCode (NonEmpty d) = Prim Auto
  ColumnCode (Vector d) = Prim Auto
  ColumnCode d = DataColumnCode (GCode d) (GCode d)

type family ColumnCodes (ds :: [*]) :: [*] where
  ColumnCodes '[] = '[]
  ColumnCodes (d : ds) = ColumnCode d : ColumnCodes ds

type family ColumnCodess (ds :: [[*]]) :: [[*]] where
  ColumnCodess '[] = '[]
  ColumnCodess (d : ds) = ColumnCodes d : ColumnCodess ds

type family TableRep d (dss :: [[*]]) :: * where
  TableRep d '[ds] = ProdTable ds
  TableRep d dss = SumAsTable d

type family ProdCode (d :: [[*]]) :: [*] where
  ProdCode '[ds] = ds
  ProdCode _ = TypeError ('Text "not a product type")

type family Rep d :: * where
  Rep d = TableRep d (ColumnCodess (GCode d))

type family NestedSum (dss :: [[*]]) :: [*] where
  NestedSum '[] = '[]
  NestedSum (ds : dss) = ProdColumn ds : NestedSum dss

type family ExplicitColumn (dt :: *) (rep :: *) (rn :: Symbol) (d :: *) (dn :: Symbol) :: * where
  ExplicitColumn dt (Sum rep) rn d dn =
    Sum (SumColumn (NestedSum (ExplicitSum d rep)))
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
    GenExplicitSum dt (GCode rep) (FieldNames rep) (GCode dt) (FieldNames dt)

type family ExplicitTable (rep :: *) (dt :: *) :: * where
  ExplicitTable Auto dt = Rep dt
  ExplicitTable rep dt = TableRep dt (ExplicitSum dt rep)

type family ReifyExplicitTable (rep :: *) (d :: *) :: * where
  ReifyExplicitTable (SumTable _) d = SumAsTable d
  ReifyExplicitTable (SumColumn _) d = SumAsTable d
  ReifyExplicitTable (ProdTable rep) _ = ProdColumn rep
  ReifyExplicitTable (ProdColumn rep) _ = ProdColumn rep
  ReifyExplicitTable rep d =
    TypeError (MessageWithType "could not reify rep for table" d <> MessageWithType ", got" rep)

type family ReifyRepTable (rep :: *) (d :: *) :: * where
  ReifyRepTable (SumTable _) d = SumAsTable d
  ReifyRepTable (SumColumn _) d = SumAsTable d
  ReifyRepTable (ProdTable rep) _ = ProdColumn rep
  ReifyRepTable (ProdColumn rep) _ = ProdColumn rep
  ReifyRepTable rep d = ReifyExplicitTable (ExplicitTable rep d) d

type family ProductForSum d :: k where
  ProductForSum d =
    ErrorWithType "cannot use product type for a sum column" d

type family ReifySumType (rep :: *) (d :: *) :: * where
  ReifySumType (SumTable _) d = SumAsTable d
  ReifySumType (SumColumn rep) d = SumColumn rep
  ReifySumType (ProdTable _) d = ProductForSum d
  ReifySumType (ProdColumn _) d = ProductForSum d
  ReifySumType rep d = SumColumn (NestedSum (ExplicitSum d rep))
