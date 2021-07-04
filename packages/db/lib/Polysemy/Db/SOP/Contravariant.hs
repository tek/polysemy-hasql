module Polysemy.Db.SOP.Contravariant where

import Generics.SOP (SListI,
  I(I),
  K(K),
  NP,
  Projection,
  hcollapse,
  hmap,
  hzipWith,
  projections,
  type (-.->)(Fn),
  unComp,
  unI,
  (:.:)(Comp),
  )

sequenceContravariantNPF ::
  ∀ (ds :: [Type]) (cv :: Type -> Type) (f :: Type -> Type) .
  SListI ds =>
  Contravariant cv =>
  (∀ a . Monoid (cv a)) =>
  NP (cv :.: f) ds ->
  cv (NP (I :.: f) ds)
sequenceContravariantNPF contrs =
  mconcat (hcollapse rows)
  where
    rows =
      hzipWith row contrs projections
    row :: ∀ a . (cv :.: f) a -> Projection f ds a -> K (cv (NP (I :.: f) ds)) a
    row (Comp par) (Fn proj) =
      K (contramap (proj . K . hmap (unI . unComp)) par)

sequenceContravariantNP ::
  ∀ (ds :: [Type]) (cv :: Type -> Type) .
  SListI ds =>
  Contravariant cv =>
  (∀ a . Monoid (cv a)) =>
  NP cv ds ->
  cv (NP I ds)
sequenceContravariantNP =
  contramap (hmap (Comp .  I)) .
  sequenceContravariantNPF .
  hmap (Comp . contramap unI)
