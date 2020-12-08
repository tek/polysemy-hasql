module Polysemy.Hasql.Table.Representation where

import qualified Chronos as Chronos
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Generics.SOP.GGP (GCode, GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (DatatypeInfo, DatatypeInfo(ADT, Newtype))
import Path (Path)
import Prelude hiding (Enum)
import Type.Errors (ErrorMessage(Text, ShowType), TypeError)
import Type.Errors.Pretty (type (<>))

import Polysemy.Db.Data.Column (Auto, Enum, Flatten, NewtypePrim, Prim, PrimaryKey, Sum)
import Polysemy.Db.Data.Uid (Uid)
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

type family SumColumnCode (dss :: [[*]]) (initial :: [[*]]) :: * where
  SumColumnCode ('[] : dss) initial =
    SumColumnCode dss initial
  SumColumnCode '[] _ =
    Enum Auto
  SumColumnCode ((_ : _) : _) initial =
    Sum Auto (SumColumn (CtorsColumnCode initial))
  SumColumnCode _ initial =
    TypeError ('Text "could not match sum type column: " <> 'ShowType initial)

type family ADTColumnCode (dss :: [[*]]) :: * where
  ADTColumnCode '[d1 : ds] =
    Flatten (ProdColumn (ColumnCodes (d1 : ds)))
  ADTColumnCode dss =
    SumColumnCode dss dss

-- TODO change NewtypePrim to NewtypeColumn or something and treat it as a transparent wrapper
type family DataColumnCode (dss :: [[*]]) (info :: DatatypeInfo) :: * where
  DataColumnCode '[ '[d] ] ('Newtype _ _ _) = NewtypePrim (ColumnCode d)
  DataColumnCode dss ('ADT _ _ _ _) = ADTColumnCode dss

type family ColumnCode (d :: *) :: * where
  ColumnCode Int = Prim Auto
  ColumnCode Text = Prim Auto
  ColumnCode ByteString = Prim Auto
  ColumnCode String = Prim Auto
  ColumnCode UUID = Prim Auto
  ColumnCode Float = Prim Auto
  ColumnCode Double = Prim Auto
  ColumnCode (Path _ _) = Prim Auto
  ColumnCode UTCTime = Prim Auto
  ColumnCode Chronos.Datetime = Prim Auto
  ColumnCode [d] = ColumnCode d
  ColumnCode (NonEmpty d) = ColumnCode d
  ColumnCode (Vector d) = ColumnCode d
  ColumnCode (Maybe d) = ColumnCode d
  ColumnCode d = DataColumnCode (GCode d) (GDatatypeInfoOf d)

type family ColumnCodes (ds :: [*]) :: [*] where
  ColumnCodes '[] = '[]
  ColumnCodes (d : ds) = ColumnCode d : ColumnCodes ds

type family ColumnCodess (ds :: [[*]]) :: [[*]] where
  ColumnCodess '[] = '[]
  ColumnCodess (d : ds) = ColumnCodes d : ColumnCodess ds

type family TableRep d (dss :: [[*]]) :: * where
  TableRep _ '[ds] = ProdTable ds
  TableRep d _ = SumAsTable d

type family ProdCode (d :: [[*]]) :: [*] where
  ProdCode '[ds] = ds
  ProdCode _ = TypeError ('Text "not a product type")

type family WithPrimaryKey (r :: *) :: * where
  WithPrimaryKey (NewtypePrim r) = NewtypePrim (WithPrimaryKey r)
  WithPrimaryKey (Prim _) = Prim PrimaryKey
  WithPrimaryKey r = r

type family Rep d :: * where
  Rep (Uid i d) = ProdTable [WithPrimaryKey (ColumnCode i), ColumnCode d]
  Rep d = TableRep d (ColumnCodess (GCode d))

type family NestedSum (dss :: [[*]]) :: [*] where
  NestedSum '[] = '[]
  NestedSum (ds : dss) = ProdColumn ds : NestedSum dss

type family ExplicitColumn (dt :: *) (rep :: *) (rn :: Symbol) (d :: *) (dn :: Symbol) :: * where
  ExplicitColumn _ (Sum rep sum) _ d _ =
    Sum rep (SumColumn (NestedSum (ExplicitSum d sum)))
  ExplicitColumn _ (Flatten rep) _ d _ =
    Flatten (ProdColumn (ProdCode (ExplicitSum d rep)))
  ExplicitColumn _ (Enum rep) _ _ _ =
    Enum rep
  ExplicitColumn _ Auto _ d _ =
    Rep d
  ExplicitColumn _ rep _ _ _ =
    rep

type family FieldMismatch (dt :: *) (rs :: [*]) (rns :: [Symbol]) (ds :: [*]) (dns :: [Symbol]) :: k where
  FieldMismatch dt _ rns '[] _ =
    TypeError ('Text "too many fields in rep for " <> 'ShowType dt <> ": " <> JoinComma rns)
  FieldMismatch dt '[] _ _ dns =
    TypeError ('Text "missing fields in rep for " <> 'ShowType dt <> ": " <> JoinComma dns)
  FieldMismatch dt _ _ _ _ =
    ErrorWithType "unexpected field mismatch when checking table representation" dt

type family ExplicitProd (dt :: *) (rs :: [*]) (rns :: [Symbol]) (ds :: [*]) (dns :: [Symbol]) :: [*] where
  ExplicitProd _ '[] _ '[] _ = '[]
  ExplicitProd dt (rep : reps) (rn : rns) (d : ds) (dn : dns) =
    ExplicitColumn dt rep rn d dn : ExplicitProd dt reps rns ds dns
  ExplicitProd dt rs rns ds dns =
    FieldMismatch dt rs rns ds dns

type family CtorMismatch dt :: k where
  CtorMismatch dt =
    ErrorWithType "constructor mismatch for database representation" dt

type Names = [[Symbol]]

type family GenExplicitSum (dt :: *) (repss :: [[*]]) (rns :: Names) (dss :: [[*]]) (dns :: Names) :: [[*]] where
  GenExplicitSum _ '[] _ '[] _ = '[]
  GenExplicitSum dt '[] _ _ _ = CtorMismatch dt
  GenExplicitSum dt _ _ '[] _ = CtorMismatch dt
  GenExplicitSum dt (rs : rss) (rn : rns) (ds : dss) (dn : dns) =
    ExplicitProd dt rs rn ds dn : GenExplicitSum dt rss rns dss dns
  GenExplicitSum dt _ _ _ _ = CtorMismatch dt

type family ExplicitSum (dt :: *) (rep :: *) :: [[*]] where
  ExplicitSum dt Auto =
    ColumnCodess (GCode dt)
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
  ReifySumType (SumColumn rep) _ = SumColumn rep
  ReifySumType (ProdTable _) d = ProductForSum d
  ReifySumType (ProdColumn _) d = ProductForSum d
  ReifySumType rep d = SumColumn (NestedSum (ExplicitSum d rep))
