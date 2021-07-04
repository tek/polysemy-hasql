{-# language CPP #-}
#define sop5 MIN_VERSION_generics_sop(0,5,0)

module Polysemy.Db.Tree.Meta where

import Fcf (Eval, FromMaybe, type (@@))
import GHC.TypeLits (AppendSymbol)
import GHC.TypeNats (type (+))
import Generics.SOP.GGP (GCode, GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (DatatypeInfo (ADT))
import Type.Errors (ErrorMessage (ShowType, Text))
import Type.Errors.Pretty (TypeError, type (%), type (<>))

import Polysemy.Db.Data.FieldId (FieldId (NamedField, NumberedField), FieldIdSymbol)
import Polysemy.Db.Data.Rep (Auto, Flatten, Product, Rep, Sum)
import Polysemy.Db.SOP.Constructor (ConstructorNames)
import Polysemy.Db.SOP.Error (ErrorWithType)
import Polysemy.Db.SOP.FieldNames (FieldIds)
import Polysemy.Db.Tree.Data.TreeMeta (ConMeta (ConMeta), TreeMeta (TreeMeta))

type Ids =
  [[FieldId]]

data AdtMetadata =
  AdtSum { cons :: [ConMeta] }
  |
  AdtProd { nodeMetas :: [TreeMeta] }
  |
  AdtEnum

data MaybeAdt =
  MaybeAdt AdtMetadata

type family NewtypePayload (dss :: [[Type]]) :: Type where
  NewtypePayload '[ '[d]] = d

type family FieldNameMismatch (d :: Type) (rn :: ErrorMessage) (dn :: ErrorMessage) :: k where
  FieldNameMismatch d rn dn =
    TypeError (
      "column name mismatch in rep for column of type " <> 'ShowType d %
      "data field is named '" <> dn <> "'" %
      "rep field is named '" <> rn <> "'"
    )

type family MatchName (d :: Type) (rn :: Symbol) (dn :: Symbol) (rn_ :: Symbol) :: Symbol where
  MatchName _ rn rn _ =
    rn
  MatchName _ rn dn dn =
    rn
  MatchName d rn dn _ =
    FieldNameMismatch d ('Text rn) ('Text dn)

type family ZipFields (reps :: [Type]) (rns :: [FieldId]) (ds :: [Type]) (dns :: [FieldId]) :: [TreeMeta] where
  ZipFields '[] '[] '[] '[] = '[]
  ZipFields (rep : reps) ('NumberedField _ n : rns) (d : ds) ('NumberedField name n : dns) =
    'TreeMeta ('NumberedField name n) rep d : ZipFields reps rns ds dns
  ZipFields (rep : reps) ('NamedField rn : rns) (d : ds) ('NamedField dn : dns) =
    'TreeMeta ('NamedField (MatchName d rn dn (AppendSymbol "_" rn))) rep d : ZipFields reps rns ds dns
  ZipFields _ (rn : _) (d : _) (dn : _) =
    FieldNameMismatch d (FieldIdSymbol @@ rn) (FieldIdSymbol @@ dn)
  ZipFields (rep : _) (rn : _) '[] '[] =
    TypeError ("Extra field '" <> (FieldIdSymbol @@ rn) <> " :: " <> 'ShowType rep <> "' in rep")
  ZipFields '[] '[] (d : _) (dn : _) =
    TypeError ("Missing field for '" <> FieldIdSymbol @@ dn <> " :: " <> 'ShowType d <> "' in rep")
  ZipFields reps rns ds dns =
    ErrorWithType "internal: ZipFields" '(reps, rns, ds, dns)

type family ADTConsGen (index :: Nat) (names :: [Symbol]) (repss :: [[Type]]) (rnss :: Ids)  (dss :: [[Type]]) (dnss :: Ids) :: [ConMeta] where
  ADTConsGen _ '[] '[] '[] '[] '[] =
    '[]
  ADTConsGen index (name : names) (reps : repss) (rns : rnss) (ds : dss) (dns : dnss) =
    'ConMeta index ('NamedField name) (ZipFields reps rns ds dns) : ADTConsGen (index + 1) names repss rnss dss dnss

type family AdtEnumGen (rep :: Type) (d :: Type) (dss :: [[Type]]) :: Maybe AdtMetadata where
  AdtEnumGen _ _ '[] =
    'Just 'AdtEnum
  AdtEnumGen rep d ('[] : dss) =
    AdtEnumGen rep d dss
  AdtEnumGen _ _ _ =
    'Nothing

type family AdtSumGen (rep :: Type) (d :: Type) (repss :: [[Type]]) (rnss :: Ids) (dss :: [[Type]]) (dnss :: Ids) :: AdtMetadata where
  AdtSumGen _ d repss rnss dss dnss =
    'AdtSum (ADTConsGen 0 (ConstructorNames d) repss rnss dss dnss)

type family AdtEnumOrSum (rep :: Type) (d :: Type) (repss :: [[Type]]) (rnss :: Ids) (dss :: [[Type]]) (dnss :: Ids) :: AdtMetadata where
  AdtEnumOrSum rep d repss rnss dss dnss =
    Eval (FromMaybe (AdtSumGen rep d repss rnss dss dnss) (AdtEnumGen rep d dss))

type family AdtMetaGen (rep :: Type) (d :: Type) (repss :: [[Type]]) (rnss :: Ids) (dss :: [[Type]]) (dnss :: Ids) :: AdtMetadata where
  AdtMetaGen _ _ '[reps] '[rns] '[ds] '[dns] =
    'AdtProd (ZipFields reps rns ds dns)
  AdtMetaGen rep d repss rnss dss dnss =
    AdtEnumOrSum rep d repss rnss dss dnss

data ADTWrap =
  ADTWrapped (Type -> Type) Type
  |
  ADTPlain Type

type family ADTRep (rep :: Type) :: Type where
  ADTRep (Rep '[]) = Auto
  ADTRep Auto = Auto
  ADTRep (Rep (Product rep : _)) = rep
  ADTRep (Rep (Sum rep : _)) = rep
  ADTRep (Rep (Flatten rep : _)) = rep
  ADTRep (Rep (_ : reps)) = ADTRep (Rep reps)
  ADTRep rep = ADTRep (Rep '[rep])

type family ProdDefaultRep (rep :: Type) :: Type where
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

type family AdtMetaExplicit (rep :: Type) (d :: Type) (meta :: DatatypeInfo) :: AdtMetadata where
#if sop5
  AdtMetaExplicit rep d ('ADT _ _ _ _) =
    AdtMetaGen rep d (GCode rep) (FieldIds rep) (GCode d) (FieldIds d)
#else
  AdtMetaExplicit rep d ('ADT _ _ _ _) =
    AdtMetaGen rep d (GCode rep) (FieldIds rep) (GCode d) (FieldIds d)
#endif
  AdtMetaExplicit rep d meta =
    ErrorWithType "AdtMetaExplicit" '(rep, d, meta)

type family AsAutoCon (ds :: [Type]) :: [Type] where
  AsAutoCon '[] = '[]
  AsAutoCon (_ : ds) = Auto : AsAutoCon ds

type family AllAuto (dss :: [[Type]]) :: [[Type]] where
  AllAuto '[] = '[]
  AllAuto (ds : dss) = AsAutoCon ds : AllAuto dss

type family AdtMetaAuto (d :: Type) (meta :: DatatypeInfo) :: AdtMetadata where
#if sop5
  AdtMetaAuto d ('ADT _ _ _ _) =
    AdtMetaGen Auto d (AllAuto (GCode d)) (FieldIds d) (GCode d) (FieldIds d)
#else
  AdtMetaAuto d ('ADT _ _ _) =
    AdtMetaGen Auto d (AllAuto (GCode d)) (FieldIds d) (GCode d) (FieldIds d)
#endif
  AdtMetaAuto d _ =
    ErrorWithType "AdtMetaAuto" d

type family NoAdt :: AdtMetadata where

type family AdtMetaAutoOrExplicit (rep :: Type) (d :: Type) :: AdtMetadata where
  AdtMetaAutoOrExplicit Auto () = NoAdt
  AdtMetaAutoOrExplicit Auto d = AdtMetaAuto d (GDatatypeInfoOf d)
  AdtMetaAutoOrExplicit rep d = AdtMetaExplicit rep d (GDatatypeInfoOf d)

type family AdtMeta' (rep :: Type) (d :: Type) :: AdtMetadata where
  AdtMeta' rep d = AdtMetaAutoOrExplicit (ADTRep rep) d

type family ForceAdtMeta (meta :: AdtMetadata) :: MaybeAdt where
  ForceAdtMeta ('AdtSum cols) = 'MaybeAdt ('AdtSum cols)
  ForceAdtMeta ('AdtProd cols) = 'MaybeAdt ('AdtProd cols)
  ForceAdtMeta 'AdtEnum = 'MaybeAdt 'AdtEnum

type family AdtMeta (rep :: Type) (d :: Type) :: MaybeAdt where
  AdtMeta rep d =
    ForceAdtMeta (AdtMeta' rep d)
