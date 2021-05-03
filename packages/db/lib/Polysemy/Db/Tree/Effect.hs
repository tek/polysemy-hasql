module Polysemy.Db.Tree.Effect where

import Prelude hiding (Enum)

import Polysemy.Db.Data.Column (Auto, Enum, Flatten, ForcePrim, ForceRep, Json, JsonB, Prim, Product, Rep, Sum)
import Polysemy.Db.SOP.HasGeneric (IsNewtype)
import Polysemy.Db.Tree.Data.Effect (ADT, Newtype, NoEffect, Tycon)
import Polysemy.Db.Tree.Meta (ADTMeta, ADTMetadata(ADTEnum), MaybeADT(MaybeADT))

newtype D =
  D *

newtype T =
  T *

newtype Effs =
  Effs [*]

----------------------------------------------------------------------------------------------------

class TreeTypeResolves (resolves :: Bool) (prim :: Bool) | resolves -> prim

instance {-# incoherent #-} prim ~ 'False => TreeTypeResolves resolves prim

instance prim ~ 'True => TreeTypeResolves 'True prim

----------------------------------------------------------------------------------------------------

class EffectfulTree (d :: *) (eff :: *) (d' :: *) | d -> eff d'

instance {-# overlappable #-} (eff ~ NoEffect, d' ~ d) => EffectfulTree d eff d'

instance EffectfulTree (Maybe d) (Tycon Maybe d) d
instance EffectfulTree [d] (Tycon [] d) d
instance EffectfulTree (NonEmpty d) (Tycon NonEmpty d) d

----------------------------------------------------------------------------------------------------

class MaybeADTResolves (adt :: MaybeADT) (meta :: Maybe ADTMetadata) | adt -> meta

instance {-# incoherent #-} meta ~ 'Nothing => MaybeADTResolves adt meta

instance meta ~ 'Just m => MaybeADTResolves ('MaybeADT m) meta

----------------------------------------------------------------------------------------------------

class IsADT (rep :: *) (d :: *) (meta :: Maybe ADTMetadata) | rep d -> meta

instance MaybeADTResolves (ADTMeta rep d) meta => IsADT rep d meta

----------------------------------------------------------------------------------------------------

type family WithEffect (e :: *) (reps :: [*]) :: [*] where
  WithEffect e '[] = '[e]
  WithEffect e (e : reps) = WithEffect e reps
  WithEffect e (rep : reps) = rep : WithEffect e reps

type WithPrim reps =
  WithEffect Prim reps

type WithEnum reps =
  WithEffect Enum reps

type MatchedADT =
  Either (*, [*]) [*]

type family RegularADT (meta :: ADTMetadata) (pre :: [*]) (reps :: [*]) (rep :: *) :: Either a [*] where
  RegularADT meta pre reps rep =
    'Right (pre ++ reps ++ '[ADT meta rep])

type family MatchADT (meta :: Maybe ADTMetadata) (pre :: [*]) (reps :: [*]) :: MatchedADT where
  MatchADT ('Just 'ADTEnum) '[] reps =
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
  Either [*] ([*], *)

type family MatchNt (inferred :: Maybe *) (pre :: [*]) (reps :: [*]) :: MatchedNt where
  MatchNt _ pre (Newtype _ wrapped : reps) =
    'Right '(pre ++ reps, wrapped)
  MatchNt inferred pre (rep : reps) =
    MatchNt inferred (pre ++ '[rep]) reps
  MatchNt ('Just wrapped) pre '[] =
    'Right '(pre, wrapped)
  MatchNt 'Nothing pre '[] =
    'Left pre

class NewtypeOrADT (nt :: MatchedNt) (d :: D) (effs :: Effs) | nt d -> effs

instance (
    ResolveRep (Rep reps) ('D wrapped) ('Effs effs)
  ) => NewtypeOrADT ('Right '(reps, wrapped)) ('D d) ('Effs (Newtype d wrapped : effs))

instance (
    IsADT (Rep reps) d meta,
    ADTOrPrim (MatchADT meta '[] reps) ('D d) effs
  ) => NewtypeOrADT ('Left reps) ('D d) effs

----------------------------------------------------------------------------------------------------

type MatchedTycon =
  Either [*] ([*], *, *)

type family MatchTycon (inferred :: *) (inner :: *) (d :: *) (pre :: [*]) (reps :: [*]) :: MatchedTycon where
  MatchTycon _ _ (f d') pre (Tycon f d' : rest) =
    'Right '(pre ++ rest, Tycon f d', d')
  MatchTycon NoEffect _ _ pre '[] =
    'Left pre
  MatchTycon eff inner _ pre '[] =
    'Right '(pre, eff, inner)
  MatchTycon eff inner d pre (rep : reps) =
    MatchTycon eff inner d (pre ++ '[rep]) reps

class TyconOrNewtype (eff :: MatchedTycon) (d :: D) (effs :: Effs) | eff d -> effs

instance (
    ResolveRep (Rep reps) ('D d') ('Effs effs)
  ) => TyconOrNewtype ('Right '(reps, eff, d')) d ('Effs (eff : effs))

instance (
    IsNewtype d nt,
    NewtypeOrADT (MatchNt nt '[] reps) ('D d) effs
  ) => TyconOrNewtype ('Left reps) ('D d) effs

----------------------------------------------------------------------------------------------------

type family MatchPrim (d :: *) (pre :: [*]) (reps :: [*]) :: Either [*] [*] where
  MatchPrim _ pre '[] = 'Left pre
  MatchPrim d pre (ForcePrim d : rest) = 'Right (WithPrim (pre ++ rest))
  MatchPrim d pre (rep : rest) = MatchPrim d (pre ++ '[rep]) rest

class PrimOrTycon (reps :: Either [*] [*]) (d :: D) (effs :: Effs) | reps d -> effs

instance PrimOrTycon ('Right reps) ('D d) ('Effs reps)

instance (
    EffectfulTree d eff d',
    TyconOrNewtype (MatchTycon eff d' d '[] reps) ('D d) effs
  ) => PrimOrTycon ('Left reps) ('D d) effs

----------------------------------------------------------------------------------------------------

class ResolveRep (reps :: *) (d :: D) (effs :: Effs) | reps d -> effs

instance (
    PrimOrTycon (MatchPrim d '[] reps) ('D d) effs
  ) => ResolveRep (Rep reps) ('D d) effs

instance ResolveRep (ForceRep reps) ('D d) ('Effs reps)

----------------------------------------------------------------------------------------------------

class ResolveTreeEffects (rep :: *) (d :: *) (effs :: [*]) | rep d -> effs

instance (
    ResolveRep (Rep '[]) ('D d) ('Effs effs)
  ) => ResolveTreeEffects Auto d effs

instance {-# overlappable #-} (
    ResolveRep (Rep rep) ('D d) ('Effs effs)
  ) => ResolveTreeEffects (Rep rep) d effs

instance {-# overlappable #-} (
    ResolveRep (Rep '[rep]) ('D d) ('Effs effs)
  ) => ResolveTreeEffects rep d effs
