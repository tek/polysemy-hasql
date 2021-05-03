{-# language CPP #-}
#define sop5 MIN_VERSION_generics_sop(0,5,0)

module Polysemy.Db.Tree.Meta where

import Fcf (Eval, FromMaybe, type (@@))
import GHC.TypeLits (AppendSymbol)
import Generics.SOP.GGP (GCode, GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (DatatypeInfo(Newtype, ADT))
import Type.Errors (ErrorMessage(ShowType, Text))
import Type.Errors.Pretty (TypeError, type (%), type (<>))

import Polysemy.Db.Data.Column (Auto, Flatten, Product, Rep, Sum)
import Polysemy.Db.Data.FieldId (FieldId(NumberedField, NamedField), FieldIdSymbol)
import Polysemy.Db.SOP.Constructor (ConstructorNames)
import Polysemy.Db.SOP.Error (ErrorWithType)
import Polysemy.Db.SOP.FieldNames (FieldIds)

type Ids =
  [[FieldId]]

data TreeMeta =
  TreeMeta {
    name :: FieldId,
    rep :: *,
    tpe :: *
  }

type family TreeMetaType (meta :: TreeMeta) :: Type where
  TreeMetaType ('TreeMeta _ _ tpe) = tpe

data ConMeta =
  ConMeta {
    conName :: FieldId,
    columns :: [TreeMeta]
  }

data ADTMetadata =
  ADTSum { cons :: [ConMeta] }
  |
  ADTProd { columns :: [TreeMeta] }
  |
  ADTEnum
  |
  ADTNewtype { wrapped :: * }

data MaybeADT =
  MaybeADT ADTMetadata

type family NewtypePayload (dss :: [[*]]) :: * where
  NewtypePayload '[ '[d]] = d

type family FieldNameMismatch (d :: *) (rn :: ErrorMessage) (dn :: ErrorMessage) :: k where
  FieldNameMismatch d rn dn =
    TypeError (
      "column name mismatch in rep for column of type " <> 'ShowType d %
      "data field is named '" <> dn <> "'" %
      "rep field is named '" <> rn <> "'"
    )

type family MatchName (d :: *) (rn :: Symbol) (dn :: Symbol) (rn_ :: Symbol) :: Symbol where
  MatchName _ rn rn _ =
    rn
  MatchName _ rn dn dn =
    rn
  MatchName d rn dn _ =
    FieldNameMismatch d ('Text rn) ('Text dn)

type family ZipFields (reps :: [*]) (rns :: [FieldId]) (ds :: [*]) (dns :: [FieldId]) :: [TreeMeta] where
  ZipFields '[] '[] '[] '[] = '[]
  ZipFields (rep : reps) ('NumberedField _ n : rns) (d : ds) ('NumberedField name n : dns) =
    'TreeMeta ('NumberedField name n) rep d : ZipFields reps rns ds dns
  ZipFields (rep : reps) ('NamedField rn : rns) (d : ds) ('NamedField dn : dns) =
    'TreeMeta ('NamedField (MatchName d rn dn (AppendSymbol "_" rn))) rep d : ZipFields reps rns ds dns
  ZipFields _ (rn : _) (d : _) (dn : _) =
    FieldNameMismatch d (FieldIdSymbol @@ rn) (FieldIdSymbol @@ dn)
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
  ADTEnumGen _ _ '[] =
    'Just 'ADTEnum
  ADTEnumGen rep d ('[] : dss) =
    ADTEnumGen rep d dss
  ADTEnumGen _ _ _ =
    'Nothing

type family ADTSumGen (rep :: *) (d :: *) (repss :: [[*]]) (rnss :: Ids) (dss :: [[*]]) (dnss :: Ids) :: ADTMetadata where
  ADTSumGen _ d repss rnss dss dnss =
    'ADTSum (ADTConsGen (ConstructorNames d) repss rnss dss dnss)

type family ADTEnumOrSum (rep :: *) (d :: *) (repss :: [[*]]) (rnss :: Ids) (dss :: [[*]]) (dnss :: Ids) :: ADTMetadata where
  ADTEnumOrSum rep d repss rnss dss dnss =
    Eval (FromMaybe (ADTSumGen rep d repss rnss dss dnss) (ADTEnumGen rep d dss))

type family ADTMetaGen (rep :: *) (d :: *) (repss :: [[*]]) (rnss :: Ids) (dss :: [[*]]) (dnss :: Ids) :: ADTMetadata where
  ADTMetaGen _ _ '[reps] '[rns] '[ds] '[dns] =
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
#if sop5
  ADTMetaExplicit rep d ('ADT _ _ _ _) =
    ADTMetaGen rep d (GCode rep) (FieldIds rep) (GCode d) (FieldIds d)
#else
  ADTMetaExplicit rep d ('ADT _ _ _ _) =
    ADTMetaGen rep d (GCode rep) (FieldIds rep) (GCode d) (FieldIds d)
#endif
  ADTMetaExplicit _ d ('Newtype _ _ _) =
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
#if sop5
  ADTMetaAuto d ('ADT _ _ _ _) =
    ADTMetaGen Auto d (AllAuto (GCode d)) (FieldIds d) (GCode d) (FieldIds d)
#else
  ADTMetaAuto d ('ADT _ _ _) =
    ADTMetaGen Auto d (AllAuto (GCode d)) (FieldIds d) (GCode d) (FieldIds d)
#endif
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
