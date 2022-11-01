module Sqel.Fold where

import Generics.SOP (All, K (K), NP, hcmap)

import Sqel.Data.Dd (Comp, CompInc, Dd (..), DdK (DdK), DdStruct (DdComp), Struct (Comp, Prim))
import Sqel.Data.Sel (Sel)

type FoldDd ::
  (Sel -> Type -> Type -> Constraint) ->
  (Comp -> CompInc -> Sel -> Sel -> Type -> Type -> [DdK] -> Constraint) ->
  Type ->
  DdK ->
  Constraint
class FoldDd pc nc result s where
  foldDd ::
    (∀ sel p a . pc sel p a => Dd ('DdK sel p a 'Prim) -> result) ->
    (
      ∀ c i sel tsel p a sub .
      nc c i sel tsel p a sub =>
      Dd ('DdK sel p a ('Comp tsel c i sub)) ->
      NP (K result) sub ->
      result
    ) ->
    Dd s ->
    result

instance (
    pc sel p a
  ) => FoldDd pc nc result ('DdK sel p a 'Prim) where
  foldDd prim _ dd = prim dd

instance (
    nc c i sel tsel p a sub,
    All (FoldDd pc nc result) sub
  ) => FoldDd pc nc result ('DdK sel p a ('Comp tsel c i sub)) where
  foldDd prim comp dd@(Dd _ _ (DdComp _ _ _ sub)) =
    comp dd (hcmap (Proxy @(FoldDd pc nc result)) (K . foldDd @pc @nc prim comp) sub)
