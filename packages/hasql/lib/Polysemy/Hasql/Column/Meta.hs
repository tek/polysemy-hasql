module Polysemy.Hasql.Column.Meta where

import Fcf (Eval, FromMaybe, type (@@))
import Generics.SOP.GGP (GCode, GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (DatatypeInfo(Newtype, ADT))
import Polysemy.Db.Data.Column (Auto, Flatten, Product, Rep, Sum)
import Polysemy.Db.Data.FieldId (FieldId(NumberedField, NamedField), FieldIdSymbol)
import Polysemy.Db.SOP.Constructor (ConstructorNames)
import Polysemy.Db.SOP.Error (ErrorWithType)
import Polysemy.Db.SOP.FieldNames (FieldNames)
import Type.Errors (ErrorMessage(ShowType))
import Type.Errors.Pretty (TypeError, type (%), type (<>))

type Ids =
  [[FieldId]]

data ColumnMeta =
  ColumnMeta {
    name :: FieldId,
    rep :: *,
    tpe :: *
  }

data ConMeta =
  ConMeta {
    conName :: FieldId,
    columns :: [ColumnMeta]
  }

data ADTMetadata =
  ADTSum { cons :: [ConMeta] }
  |
  ADTProd { columns :: [ColumnMeta] }
  |
  ADTEnum
  |
  ADTNewtype { wrapped :: * }

data MaybeADT =
  MaybeADT ADTMetadata

type family NewtypePayload (dss :: [[*]]) :: * where
  NewtypePayload '[ '[d]] = d

type family ZipFields (reps :: [*]) (rns :: [FieldId]) (ds :: [*]) (dns :: [FieldId]) :: [ColumnMeta] where
  ZipFields '[] '[] '[] '[] = '[]
  ZipFields (rep : reps) ('NumberedField _ n : rns) (d : ds) ('NumberedField name n : dns) =
    'ColumnMeta ('NumberedField name n) rep d : ZipFields reps rns ds dns
  ZipFields (rep : reps) (name : rns) (d : ds) (name : dns) =
    'ColumnMeta name rep d : ZipFields reps rns ds dns
  ZipFields _ (rn : _) (d : _) (dn : _) =
    TypeError (
      "column name mismatch in rep for column of type " <> 'ShowType d %
      "data field is named '" <> FieldIdSymbol @@ dn <> "'" %
      "rep field is named '" <> FieldIdSymbol @@ rn <> "'"
    )
  ZipFields (rep : _) (rn : _) '[] '[] =
    TypeError ("Extra field '" <> rn <> " :: " <> 'ShowType rep <> "' in rep")
  ZipFields '[] '[] (d : _) (dn : _) =
    TypeError ("Missing field for '" <> FieldIdSymbol @@ dn <> " :: " <> 'ShowType d <> "' in rep")
  ZipFields reps rns ds dns =
    ErrorWithType "internal: ZipFields" '(reps, rns, ds, dns)

type family ADTConsGen (names :: [Symbol]) (repss :: [[*]]) (rnss :: Ids)  (dss :: [[*]]) (dnss :: Ids) :: [ConMeta] where
  ADTConsGen '[] '[] '[] '[] '[] =
    '[]
  ADTConsGen (name : names) (reps : repss) (rns : rnss) (ds : dss) (dns : dnss) =
    'ConMeta ('NamedField name) (ZipFields reps rns ds dns) : ADTConsGen names repss rnss dss dnss

type family ADTEnumGen (rep :: *) (d :: *) (dss :: [[*]]) :: Maybe ADTMetadata where
  ADTEnumGen rep d '[] =
    'Just 'ADTEnum
  ADTEnumGen rep d ('[] : dss) =
    ADTEnumGen rep d dss
  ADTEnumGen _ _ _ =
    'Nothing

type family ADTSumGen (rep :: *) (d :: *) (repss :: [[*]]) (rnss :: Ids) (dss :: [[*]]) (dnss :: Ids) :: ADTMetadata where
  ADTSumGen rep d repss rnss dss dnss =
    'ADTSum (ADTConsGen (ConstructorNames d) repss rnss dss dnss)

type family ADTEnumOrSum (rep :: *) (d :: *) (repss :: [[*]]) (rnss :: Ids) (dss :: [[*]]) (dnss :: Ids) :: ADTMetadata where
  ADTEnumOrSum rep d repss rnss dss dnss =
    Eval (FromMaybe (ADTSumGen rep d repss rnss dss dnss) (ADTEnumGen rep d dss))

type family ADTMetaGen (rep :: *) (d :: *) (repss :: [[*]]) (rnss :: Ids) (dss :: [[*]]) (dnss :: Ids) :: ADTMetadata where
  ADTMetaGen rep d '[reps] '[rns] '[ds] '[dns] =
    'ADTProd (ZipFields reps rns ds dns)
  ADTMetaGen rep d repss rnss dss dnss =
    ADTEnumOrSum rep d repss rnss dss dnss

data ADTWrap =
  ADTWrapped (* -> *) *
  |
  ADTPlain *

type family ADTRep (rep :: *) :: * where
  ADTRep (Rep '[]) = Auto
  ADTRep Auto = Auto
  ADTRep (Rep (Product rep : _)) = rep
  ADTRep (Rep (Sum rep : _)) = rep
  ADTRep (Rep (Flatten rep : _)) = rep
  ADTRep (Rep (_ : reps)) = ADTRep (Rep reps)
  ADTRep rep = ADTRep (Rep '[rep])

type family ProdDefaultRep (rep :: *) :: * where
  ProdDefaultRep Auto =
    Auto
  ProdDefaultRep (Rep rep) =
    Rep rep
  ProdDefaultRep (Product rep) =
    Rep '[Product rep]
  ProdDefaultRep (Flatten rep) =
    Rep '[Flatten rep]
  ProdDefaultRep (Sum rep) =
    Rep '[Sum rep]
  ProdDefaultRep rep =
    Rep '[Product rep]

type family ADTMetaExplicit (rep :: *) (d :: *) (meta :: DatatypeInfo) :: ADTMetadata where
  ADTMetaExplicit rep d ('ADT _ _ _ _) =
    ADTMetaGen rep d (GCode rep) (FieldNames rep) (GCode d) (FieldNames d)
  ADTMetaExplicit rep d ('Newtype _ _ _) =
    'ADTNewtype (NewtypePayload (GCode d))
  ADTMetaExplicit rep d meta =
    ErrorWithType "ADTMetaExplicit" '(rep, d, meta)

type family AsAutoCon (ds :: [*]) :: [*] where
  AsAutoCon '[] = '[]
  AsAutoCon (_ : ds) = Auto : AsAutoCon ds

type family AllAuto (dss :: [[*]]) :: [[*]] where
  AllAuto '[] = '[]
  AllAuto (ds : dss) = AsAutoCon ds : AllAuto dss

type family ADTMetaAuto (d :: *) (meta :: DatatypeInfo) :: ADTMetadata where
  ADTMetaAuto d ('ADT _ _ _ _) =
    ADTMetaGen Auto d (AllAuto (GCode d)) (FieldNames d) (GCode d) (FieldNames d)
  ADTMetaAuto d ('Newtype _ _ _) =
    'ADTNewtype (NewtypePayload (GCode d))
  ADTMetaAuto d _ =
    ErrorWithType "ADTMetaAuto" d

type family ADTMetaAutoOrExplicit (rep :: *) (d :: *) :: ADTMetadata where
  ADTMetaAutoOrExplicit Auto d = ADTMetaAuto d (GDatatypeInfoOf d)
  ADTMetaAutoOrExplicit rep d = ADTMetaExplicit rep d (GDatatypeInfoOf d)

type family ADTMeta' (rep :: *) (d :: *) :: ADTMetadata where
  ADTMeta' rep d = ADTMetaAutoOrExplicit (ADTRep rep) d

type family ForceADTMeta (meta :: ADTMetadata) :: MaybeADT where
  ForceADTMeta ('ADTSum cols) = 'MaybeADT ('ADTSum cols)
  ForceADTMeta ('ADTProd cols) = 'MaybeADT ('ADTProd cols)
  ForceADTMeta 'ADTEnum = 'MaybeADT 'ADTEnum
  ForceADTMeta ('ADTNewtype d) = 'MaybeADT ('ADTNewtype d)

type family ADTMeta (rep :: *) (d :: *) :: MaybeADT where
  ADTMeta rep d =
    ForceADTMeta (ADTMeta' rep d)
