module Polysemy.Db.Tree.Effect where

import Prelude hiding (Enum)

import Polysemy.Db.Data.Column (Auto, Enum, Flatten, ForcePrim, ForceRep, Json, JsonB, Prim, Product, Rep, Sum)
import Polysemy.Db.SOP.HasGeneric (IsNewtype)
import Polysemy.Db.Tree.Data.Effect (ADT, Newtype, NoEffect, Tycon)
import Polysemy.Db.Tree.Meta (ADTMeta, AdtMetadata(AdtEnum), MaybeADT(MaybeADT))

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

----------------------------------------------------------------------------------------------------

class MaybeADTResolves (adt :: MaybeADT) (meta :: Maybe AdtMetadata) | adt -> meta

instance {-# incoherent #-} meta ~ 'Nothing => MaybeADTResolves adt meta

instance meta ~ 'Just m => MaybeADTResolves ('MaybeADT m) meta

----------------------------------------------------------------------------------------------------

class IsADT (rep :: Type) (d :: Type) (meta :: Maybe AdtMetadata) | rep d -> meta

instance MaybeADTResolves (ADTMeta rep d) meta => IsADT rep d meta

----------------------------------------------------------------------------------------------------

type family WithEffect (e :: Type) (reps :: [Type]) :: [Type] where
  WithEffect e '[] = '[e]
  WithEffect e (e : reps) = WithEffect e reps
  WithEffect e (rep : reps) = rep : WithEffect e reps

type WithPrim reps =
  WithEffect Prim reps

type WithEnum reps =
  WithEffect Enum reps

type MatchedADT =
  Either (*, [Type]) [Type]

type family ExtractAdtRep (effs :: [*]) :: * where
  ExtractAdtRep (ADT _ rep : _) = rep
  ExtractAdtRep (_ : effs) = ExtractAdtRep effs

type family RegularADT (meta :: AdtMetadata) (pre :: [Type]) (reps :: [Type]) (rep :: Type) :: Either a [Type] where
  RegularADT meta pre reps rep =
    'Right (pre ++ reps ++ '[ADT meta rep])

type family MatchADT (meta :: Maybe AdtMetadata) (pre :: [Type]) (reps :: [Type]) :: MatchedADT where
  MatchADT ('Just 'AdtEnum) '[] reps =
    'Right (Enum : reps)
  MatchADT ('Just meta) pre '[] =
    'Right (ADT meta Auto : pre)
  MatchADT 'Nothing pre '[] =
    'Left '(Prim, pre)
  MatchADT ('Just meta) pre (Product rep : reps) =
    RegularADT meta pre reps (Product rep)
  MatchADT ('Just meta) pre (Flatten rep : reps) =
    RegularADT meta pre reps (Flatten rep)
  MatchADT ('Just meta) pre (Sum rep : reps) =
    RegularADT meta pre reps (Sum rep)
  MatchADT _ pre (Json : reps) =
    'Left '(Json, pre ++ reps)
  MatchADT _ pre (JsonB : reps) =
    'Left '(JsonB, pre ++ reps)
  MatchADT _ pre (Prim : reps) =
    'Left '(Prim, pre ++ reps)
  MatchADT meta pre (rep : reps) =
    MatchADT meta (pre ++ '[rep]) reps

class ADTOrPrim (adt :: MatchedADT) (d :: D) (effs :: Effs) | adt d -> effs

instance ADTOrPrim ('Right reps) ('D d) ('Effs reps)

instance withPrim ~ WithEffect prim reps => ADTOrPrim ('Left '(prim, reps)) ('D d) ('Effs withPrim)

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

class NewtypeOrADT (tag :: Type) (nt :: MatchedNt) (d :: D) (effs :: Effs) | nt d -> effs

instance (
    ResolveRep tag (Rep reps) ('D wrapped) ('Effs effs)
  ) => NewtypeOrADT tag ('Right '(reps, wrapped)) ('D d) ('Effs (Newtype d wrapped : effs))

instance (
    IsADT (Rep reps) d meta,
    ADTOrPrim (MatchADT meta '[] reps) ('D d) effs
  ) => NewtypeOrADT tag ('Left reps) ('D d) effs

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
    NewtypeOrADT tag (MatchNt nt '[] reps) ('D d) effs
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
    PrimOrTycon tag (MatchPrim d '[] reps) ('D d) effs
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

class TreeEffects (tag :: Type) (rep :: Type) (d :: Type) (effs :: [Type]) | rep d -> effs

instance TreeEffectsFor DefaultEffects rep d effs => TreeEffects DefaultEffects rep d effs
