{-# language CPP #-}

module Polysemy.Db.Tree.Effect where

import Data.Vector (Vector)
import Path (Path)
import Prelude hiding (Enum)

import Polysemy.Db.Data.Rep (Auto, Enum, Flatten, ForcePrim, ForceRep, Json, JsonB, Prim, Product, Rep, Sum)
import Polysemy.Db.SOP.HasGeneric (IsNewtype)
import Polysemy.Db.Tree.Data.Effect (Adt, Newtype, NoEffect, Tycon)
import Polysemy.Db.Tree.Meta (AdtMeta, AdtMetadata (AdtEnum), MaybeAdt (MaybeAdt))

newtype D =
  D Type

newtype Effs =
  Effs [Type]

data DefaultEffects

----------------------------------------------------------------------------------------------------

class EffectfulTree (d :: Type) (eff :: Type) (d' :: Type) | d -> eff d'

instance {-# overlappable #-} (eff ~ NoEffect, d' ~ d) => EffectfulTree d eff d'

instance EffectfulTree (Maybe d) (Tycon Maybe d) d
instance EffectfulTree [d] (Tycon [] d) d
instance EffectfulTree (NonEmpty d) (Tycon NonEmpty d) d
instance EffectfulTree (Vector d) (Tycon Vector d) d
instance EffectfulTree (Set d) (Tycon Set d) d

----------------------------------------------------------------------------------------------------

class MaybeAdtResolves (adt :: MaybeAdt) (meta :: Maybe AdtMetadata) | adt -> meta

instance {-# incoherent #-} meta ~ 'Nothing => MaybeAdtResolves adt meta

instance meta ~ 'Just m => MaybeAdtResolves ('MaybeAdt m) meta

----------------------------------------------------------------------------------------------------

class IsAdt (rep :: Type) (d :: Type) (meta :: Maybe AdtMetadata) | rep d -> meta

instance MaybeAdtResolves (AdtMeta rep d) meta => IsAdt rep d meta

----------------------------------------------------------------------------------------------------

type family WithEffect (e :: Type) (reps :: [Type]) :: [Type] where
  WithEffect e '[] = '[e]
  WithEffect e (e : reps) = WithEffect e reps
  WithEffect e (rep : reps) = rep : WithEffect e reps

type WithPrim reps =
  WithEffect Prim reps

type WithEnum reps =
  WithEffect Enum reps

type MatchedAdt =
  Either (Type, [Type]) [Type]

type family ExtractAdtRep (effs :: [Type]) :: Type where
  ExtractAdtRep (Adt _ rep : _) = rep
  ExtractAdtRep (_ : effs) = ExtractAdtRep effs

type family RegularAdt (meta :: AdtMetadata) (pre :: [Type]) (reps :: [Type]) (rep :: Type) :: Either a [Type] where
  RegularAdt meta pre reps rep =
    'Right (pre ++ reps ++ '[Adt meta rep])

type family MatchAdt (meta :: Maybe AdtMetadata) (pre :: [Type]) (reps :: [Type]) :: MatchedAdt where
  MatchAdt ('Just 'AdtEnum) '[] reps =
    'Right (Enum : reps)
  MatchAdt ('Just meta) pre '[] =
    'Right (Adt meta Auto : pre)
  MatchAdt 'Nothing pre '[] =
    'Left '(Prim, pre)
  MatchAdt ('Just meta) pre (Product rep : reps) =
    RegularAdt meta pre reps (Product rep)
  MatchAdt ('Just meta) pre (Flatten rep : reps) =
    RegularAdt meta pre reps (Flatten rep)
  MatchAdt ('Just meta) pre (Sum rep : reps) =
    RegularAdt meta pre reps (Sum rep)
  MatchAdt _ pre (Json : reps) =
    'Left '(Json, pre ++ reps)
  MatchAdt _ pre (JsonB : reps) =
    'Left '(JsonB, pre ++ reps)
  MatchAdt _ pre (Prim : reps) =
    'Left '(Prim, pre ++ reps)
  MatchAdt meta pre (rep : reps) =
    MatchAdt meta (pre ++ '[rep]) reps

class AdtOrPrim (adt :: MatchedAdt) (d :: D) (effs :: Effs) | adt d -> effs

instance AdtOrPrim ('Right reps) ('D d) ('Effs reps)

instance withPrim ~ WithEffect prim reps => AdtOrPrim ('Left '(prim, reps)) ('D d) ('Effs withPrim)

----------------------------------------------------------------------------------------------------

class NewtypeIsPrim (d :: Type) where
  type NewtypeIsPrimResolves d :: Bool
#if MIN_VERSION_GLASGOW_HASKELL(8,10,0,0)
  type NewtypeIsPrimResolves _ = 'True
#else
  type NewtypeIsPrimResolves d = 'True
#endif

instance NewtypeIsPrim (Path b t)

class NewtypeOrPrim (tag :: Type) (defined :: Bool) (d :: Type) (wrapped :: Type) (reps :: [Type]) (effs :: Effs) | defined d wrapped reps -> effs

instance effs ~ 'Effs '[Prim] => NewtypeOrPrim tag 'True d wrapped '[] effs

instance {-# incoherent #-} (
    effs' ~ 'Effs (Newtype d wrapped : effs),
    ResolveRep tag (Rep reps) ('D wrapped) ('Effs effs)
  ) => NewtypeOrPrim tag defined d wrapped reps effs'

----------------------------------------------------------------------------------------------------

type MatchedNt =
  Either [Type] ([Type], Type)

type family MatchNt (inferred :: Maybe Type) (pre :: [Type]) (reps :: [Type]) :: MatchedNt where
  MatchNt _ pre (Newtype _ wrapped : reps) =
    'Right '(pre ++ reps, wrapped)
  MatchNt inferred pre (rep : reps) =
    MatchNt inferred (pre ++ '[rep]) reps
  MatchNt ('Just wrapped) pre '[] =
    'Right '(pre, wrapped)
  MatchNt 'Nothing pre '[] =
    'Left pre

class NewtypeOrAdt (tag :: Type) (nt :: MatchedNt) (d :: D) (effs :: Effs) | nt d -> effs

instance (
    NewtypeOrPrim tag (NewtypeIsPrimResolves d) d wrapped reps effs
  ) => NewtypeOrAdt tag ('Right '(reps, wrapped)) ('D d) effs

instance (
    IsAdt (Rep reps) d meta,
    AdtOrPrim (MatchAdt meta '[] reps) ('D d) effs
  ) => NewtypeOrAdt tag ('Left reps) ('D d) effs

----------------------------------------------------------------------------------------------------

type MatchedTycon =
  Either [Type] ([Type], Type, Type)

type family MatchTycon (inferred :: Type) (inner :: Type) (d :: Type) (pre :: [Type]) (reps :: [Type]) :: MatchedTycon where
  MatchTycon _ _ (f d') pre (Tycon f d' : rest) =
    'Right '(pre ++ rest, Tycon f d', d')
  MatchTycon NoEffect _ _ pre '[] =
    'Left pre
  MatchTycon eff inner _ pre '[] =
    'Right '(pre, eff, inner)
  MatchTycon eff inner d pre (rep : reps) =
    MatchTycon eff inner d (pre ++ '[rep]) reps

class TyconOrNewtype (tag :: Type) (eff :: MatchedTycon) (d :: D) (effs :: Effs) | eff d -> effs

instance (
    ResolveRep tag (Rep reps) ('D d') ('Effs effs)
  ) => TyconOrNewtype tag ('Right '(reps, eff, d')) d ('Effs (eff : effs))

instance (
    IsNewtype d nt,
    NewtypeOrAdt tag (MatchNt nt '[] reps) ('D d) effs
  ) => TyconOrNewtype tag ('Left reps) ('D d) effs

----------------------------------------------------------------------------------------------------

type family MatchPrim (d :: Type) (pre :: [Type]) (reps :: [Type]) :: Either [Type] [Type] where
  MatchPrim _ pre '[] = 'Left pre
  MatchPrim d pre (ForcePrim d : rest) = 'Right (WithPrim (pre ++ rest))
  MatchPrim d pre (rep : rest) = MatchPrim d (pre ++ '[rep]) rest

class PrimOrTycon (tag :: Type) (reps :: Either [Type] [Type]) (d :: D) (effs :: Effs) | reps d -> effs

instance PrimOrTycon tag ('Right reps) ('D d) ('Effs reps)

instance (
    EffectfulTree d eff d',
    TyconOrNewtype tag (MatchTycon eff d' d '[] reps) ('D d) effs
  ) => PrimOrTycon tag ('Left reps) ('D d) effs

----------------------------------------------------------------------------------------------------

class ResolveRep (tag :: Type) (reps :: Type) (d :: D) (effs :: Effs) | reps d -> effs

instance (
    PrimOrTycon DefaultEffects (MatchPrim d '[] reps) ('D d) effs
  ) => ResolveRep DefaultEffects (Rep reps) ('D d) effs

instance ResolveRep tag (ForceRep reps) ('D d) ('Effs reps)

----------------------------------------------------------------------------------------------------

class TreeEffectsFor (tag :: Type) (rep :: Type) (d :: Type) (effs :: [Type]) | tag rep d -> effs

instance (
    ResolveRep tag (Rep '[]) ('D d) ('Effs effs)
  ) => TreeEffectsFor tag Auto d effs

instance {-# overlappable #-} (
    ResolveRep tag (Rep rep) ('D d) ('Effs effs)
  ) => TreeEffectsFor tag (Rep rep) d effs

instance {-# overlappable #-} (
    ResolveRep tag (Rep '[rep]) ('D d) ('Effs effs)
  ) => TreeEffectsFor tag rep d effs

class TreeEffects (tag :: Type) (rep :: Type) (d :: Type) (effs :: [Type]) | tag rep d -> effs

instance TreeEffectsFor DefaultEffects rep d effs => TreeEffects DefaultEffects rep d effs
