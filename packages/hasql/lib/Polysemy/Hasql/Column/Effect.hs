module Polysemy.Hasql.Column.Effect where

import Data.Vector (Vector)
import Polysemy.Db.Data.Column (Auto, Enum, Flatten, ForcePrim, ForceRep, Json, JsonB, Prim, Product, Rep, Sum)
import Polysemy.Db.SOP.HasGeneric (IsNewtype)
import Polysemy.Db.Tree.Data.Effect (ADT, Newtype, NoEffect, Tycon)
import Polysemy.Db.Tree.Meta (ADTMeta, AdtMetadata(AdtEnum), MaybeADT(MaybeADT))
import Prelude hiding (Enum)

import Polysemy.Hasql.ColumnType (ColumnTypeDefined)

newtype D =
  D *

newtype Effs =
  Effs [*]

----------------------------------------------------------------------------------------------------

class ColumnTypeResolves (resolves :: Bool) (prim :: Bool) | resolves -> prim

instance {-# incoherent #-} prim ~ 'False => ColumnTypeResolves resolves prim

instance prim ~ 'True => ColumnTypeResolves 'True prim

----------------------------------------------------------------------------------------------------

class PrimColumn (d :: *) (decision :: Bool) | d -> decision

instance ColumnTypeResolves (ColumnTypeDefined d) decision => PrimColumn d decision

----------------------------------------------------------------------------------------------------

class EffectfulColumn (d :: *) (eff :: *) (d' :: *) | d -> eff d'

instance {-# overlappable #-} (eff ~ NoEffect, d' ~ d) => EffectfulColumn d eff d'

instance EffectfulColumn (Maybe d) (Tycon Maybe d) d
instance EffectfulColumn [d] (Tycon [] d) d
instance EffectfulColumn (NonEmpty d) (Tycon NonEmpty d) d
instance EffectfulColumn (Vector d) (Tycon Vector d) d

----------------------------------------------------------------------------------------------------

class MaybeADTResolves (adt :: MaybeADT) (meta :: Maybe AdtMetadata) | adt -> meta

instance {-# incoherent #-} meta ~ 'Nothing => MaybeADTResolves adt meta

instance meta ~ 'Just m => MaybeADTResolves ('MaybeADT m) meta

----------------------------------------------------------------------------------------------------

class IsADT (rep :: *) (d :: *) (meta :: Maybe AdtMetadata) | rep d -> meta

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

type family RegularADT (meta :: AdtMetadata) (pre :: [*]) (reps :: [*]) (rep :: *) :: Either a [*] where
  RegularADT meta pre reps rep =
    'Right (pre ++ reps ++ '[ADT meta rep])

type family MatchADT (meta :: Maybe AdtMetadata) (pre :: [*]) (reps :: [*]) :: MatchedADT where
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

type family MatchPrim (global :: Bool) (d :: *) (pre :: [*]) (reps :: [*]) :: Either [*] [*] where
  MatchPrim 'True _ pre '[] = 'Right (WithPrim pre)
  MatchPrim 'False _ pre '[] = 'Left pre
  MatchPrim _ d pre (ForcePrim d : rest) = 'Right (WithPrim (pre ++ rest))
  MatchPrim global d pre (rep : rest) = MatchPrim global d (pre ++ '[rep]) rest

class PrimOrTycon (reps :: Either [*] [*]) (d :: D) (effs :: Effs) | reps d -> effs

instance PrimOrTycon ('Right reps) ('D d) ('Effs reps)

instance (
    EffectfulColumn d eff d',
    TyconOrNewtype (MatchTycon eff d' d '[] reps) ('D d) effs
  ) => PrimOrTycon ('Left reps) ('D d) effs

----------------------------------------------------------------------------------------------------

class ResolveRep (reps :: *) (d :: D) (effs :: Effs) | reps d -> effs

instance (
    PrimColumn d prim,
    PrimOrTycon (MatchPrim prim d '[] reps) ('D d) effs
  ) => ResolveRep (Rep reps) ('D d) effs

instance ResolveRep (ForceRep reps) ('D d) ('Effs reps)

----------------------------------------------------------------------------------------------------

class ResolveColumnEffects (rep :: *) (d :: *) (effs :: [*]) | rep d -> effs

instance (
    ResolveRep (Rep '[]) ('D d) ('Effs effs)
  ) => ResolveColumnEffects Auto d effs

instance {-# overlappable #-} (
    ResolveRep (Rep rep) ('D d) ('Effs effs)
  ) => ResolveColumnEffects (Rep rep) d effs

instance {-# overlappable #-} (
    ResolveRep (Rep '[rep]) ('D d) ('Effs effs)
  ) => ResolveColumnEffects rep d effs
