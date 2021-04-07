module Polysemy.Hasql.Column.Effect where

import Data.Vector (Vector)
import Polysemy.Db.Data.Column (Auto, Enum, Flatten, ForcePrim, ForceRep, Json, JsonB, Prim, Product, Rep, Sum)
import Polysemy.Db.SOP.HasGeneric (IsNewtype)
import Prelude hiding (Enum)

import Polysemy.Hasql.Column.Data.Effect (ADT, Newtype, NoEffect, Tc)
import Polysemy.Hasql.Column.Meta (ADTMeta, ADTMetadata(ADTEnum), MaybeADT(MaybeADT))
import Polysemy.Hasql.ColumnType (ColumnTypeDefined)

newtype D =
  D *

newtype T =
  T *

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

instance EffectfulColumn (Maybe d) (Tc Maybe d) d
instance EffectfulColumn [d] (Tc [] d) d
instance EffectfulColumn (NonEmpty d) (Tc NonEmpty d) d
instance EffectfulColumn (Vector d) (Tc Vector d) d

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

class ADTOrPrim (adt :: MatchedADT) (d :: D) (effs :: Effs) (t :: T) | adt d -> effs t

instance ADTOrPrim ('Right reps) ('D d) ('Effs reps) ('T d)

instance withPrim ~ WithEffect prim reps => ADTOrPrim ('Left '(prim, reps)) ('D d) ('Effs withPrim) ('T d)

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

class NewtypeOrADT (nt :: MatchedNt) (d :: D) (effs :: Effs) (t :: T) | nt d -> effs t

instance (
    ResolveRep (Rep reps) ('D wrapped) ('Effs effs) t
  ) => NewtypeOrADT ('Right '(reps, wrapped)) ('D d) ('Effs (Newtype d wrapped : effs)) t

instance (
    IsADT (Rep reps) d meta,
    ADTOrPrim (MatchADT meta '[] reps) ('D d) effs t
  ) => NewtypeOrADT ('Left reps) ('D d) effs t

----------------------------------------------------------------------------------------------------

type MatchedTc =
  Either [*] ([*], *, *)

type family MatchTc (inferred :: *) (inner :: *) (d :: *) (pre :: [*]) (reps :: [*]) :: MatchedTc where
  MatchTc _ _ (f d') pre (Tc f d' : rest) =
    'Right '(pre ++ rest, Tc f d', d')
  MatchTc NoEffect _ _ pre '[] =
    'Left pre
  MatchTc eff inner _ pre '[] =
    'Right '(pre, eff, inner)
  MatchTc eff inner d pre (rep : reps) =
    MatchTc eff inner d (pre ++ '[rep]) reps

class TcOrNewtype (eff :: MatchedTc) (d :: D) (effs :: Effs) (t :: T) | eff d -> effs t

instance (
    ResolveRep (Rep reps) ('D d') ('Effs effs) t
  ) => TcOrNewtype ('Right '(reps, eff, d')) d ('Effs (eff : effs)) t

instance (
    IsNewtype d nt,
    NewtypeOrADT (MatchNt nt '[] reps) ('D d) effs t
  ) => TcOrNewtype ('Left reps) ('D d) effs t

----------------------------------------------------------------------------------------------------

type family MatchPrim (global :: Bool) (d :: *) (pre :: [*]) (reps :: [*]) :: Either [*] [*] where
  MatchPrim 'True _ pre '[] = 'Right (WithPrim pre)
  MatchPrim 'False _ pre '[] = 'Left pre
  MatchPrim _ d pre (ForcePrim d : rest) = 'Right (WithPrim (pre ++ rest))
  MatchPrim global d pre (rep : rest) = MatchPrim global d (pre ++ '[rep]) rest

class PrimOrTc (reps :: Either [*] [*]) (d :: D) (effs :: Effs) (t :: T) | reps d -> effs t

instance PrimOrTc ('Right reps) ('D d) ('Effs reps) ('T d)

instance (
    EffectfulColumn d eff d',
    TcOrNewtype (MatchTc eff d' d '[] reps) ('D d) effs t
  ) => PrimOrTc ('Left reps) ('D d) effs t

----------------------------------------------------------------------------------------------------

class ResolveRep (reps :: *) (d :: D) (effs :: Effs) (t :: T) | reps d -> effs t

instance (
    PrimColumn d prim,
    PrimOrTc (MatchPrim prim d '[] reps) ('D d) effs t
  ) => ResolveRep (Rep reps) ('D d) effs t

instance ResolveRep (ForceRep reps) ('D d) ('Effs reps) ('T d)

----------------------------------------------------------------------------------------------------

class ResolveColumnEffects (rep :: *) (d :: *) (effs :: [*]) (t :: *) | rep d -> effs t

instance (
  ResolveRep (Rep '[]) ('D d) ('Effs effs) ('T t)
  ) => ResolveColumnEffects Auto d effs t

instance {-# overlappable #-} (
    ResolveRep (Rep rep) ('D d) ('Effs effs) ('T t)
  ) => ResolveColumnEffects (Rep rep) d effs t

instance {-# overlappable #-} (
    ResolveRep (Rep '[rep]) ('D d) ('Effs effs) ('T t)
  ) => ResolveColumnEffects rep d effs t
