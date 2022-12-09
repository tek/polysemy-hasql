module Sqel.Class.Mods where

import Generics.SOP (I (I), NP (Nil, (:*)))
import Prelude hiding (Compose)

import Sqel.Data.Dd (Dd (Dd), DdK (DdK), (:>) ((:>)))
import Sqel.Data.Mods (Mods (Mods), unMods)

class SymNP p ps where
  symNP :: p -> NP I ps

instance {-# overlappable #-} (
    ps ~ '[p]
  ) => SymNP p ps where
  symNP p =
    I p :* Nil

instance (
    SymNP p1 ps
  ) => SymNP (p0 :> p1) (p0 : ps) where
  symNP (p0 :> p1) =
    I p0 :* symNP p1

instance SymNP (NP I ps) ps where
  symNP = id

symMods ::
  SymNP p ps =>
  p ->
  Mods ps
symMods p =
  Mods (symNP p)

class MapMod' p ps0 ps1 | p ps0 -> ps1 where
  mapMod' :: p -> (p -> p) -> Mods ps0 -> Mods ps1

instance MapMod' p (p : ps) (p : ps) where
  mapMod' _ f (Mods (I p :* ps)) =
    Mods (I (f p) :* ps)

instance (
    MapMod' p ps0 ps1
  ) => MapMod' p (a' : ps0) (a' : ps1) where
    mapMod' p f (Mods (a' :* ps)) =
      Mods (a' :* unMods (mapMod' p f (Mods ps)))

instance MapMod' p '[] '[p] where
  mapMod' p f (Mods Nil) =
    Mods (I (f p) :* Nil)

amendMod' ::
  MapMod' p ps0 ps1 =>
  p ->
  Mods ps0 ->
  Mods ps1
amendMod' p =
  mapMod' p id

setMod' ::
  MapMod' p ps0 ps1 =>
  p ->
  Mods ps0 ->
  Mods ps1
setMod' p =
  mapMod' p (const p)

-- TODO this could map over multiple matching mods
class OverMod' p ps where
  overMod' :: (p -> p) -> Mods ps -> Mods ps

instance OverMod' p (p : ps) where
  overMod' f (Mods (I p :* ps)) =
    Mods (I (f p) :* ps)

instance (
    OverMod' p ps
  ) => OverMod' p (a' : ps) where
    overMod' f (Mods (a' :* ps)) =
      Mods (a' :* unMods (overMod' f (Mods ps)))

instance OverMod' p '[] where
  overMod' _ (Mods Nil) =
    Mods Nil

class CMapMod' c p0 p p1 ps0 ps1 | ps0 p0 p1 -> p ps1 where
  cmapMod' :: p0 -> (c p p1 => p -> p1) -> Mods ps0 -> Mods ps1

instance (
    c p p1
  ) => CMapMod' c p0 p p1 (p : ps) (p1 : ps) where
  cmapMod' _ f (Mods (I p :* ps)) =
    Mods (I (f p) :* ps)

instance (
    CMapMod' c p0 p p1 ps0 ps1
  ) => CMapMod' c p0 p p1 (a' : ps0) (a' : ps1) where
    cmapMod' p f (Mods (a' :* ps)) =
      Mods (a' :* unMods (cmapMod' @c @p0 @p @p1 @ps0 @ps1 p f (Mods ps)))

instance CMapMod' c p0 p1 p1 '[] '[p0] where
  cmapMod' p _ (Mods Nil) =
    Mods (I p :* Nil)

type GetMod :: Constraint -> Type -> [Type] -> Constraint
class GetMod c p ps where
  getMod :: (c => p) -> Mods ps -> p

instance c => GetMod c p '[] where
  getMod f (Mods Nil) = f

instance GetMod c p (p : ps) where
  getMod _ (Mods (I p :* _)) = p

class AddMod p s0 s1 | p s0 -> s1 where
  addMod :: p -> Dd s0 -> Dd s1

instance AddMod p ('DdK sel ps a s) ('DdK sel (p : ps) a s) where
  addMod p (Dd sel (Mods ps) s) =
    Dd sel (Mods (I p :* ps)) s

instance {-# overlappable #-} (
    GetMod c p ps
  ) => GetMod c p (a' : ps) where
    getMod f (Mods (_ :* ps)) =
      getMod @c f (Mods ps)

class MapMod p s0 s1 | p s0 -> s1 where
  mapMod :: p -> (p -> p) -> Dd s0 -> Dd s1

instance (
    MapMod' p ps0 ps1
  ) => MapMod p ('DdK sel ps0 a s0) ('DdK sel ps1 a s0) where
    mapMod p f (Dd sel ps0 s) =
      Dd sel (mapMod' p f ps0) s

class OverMod p s where
  overMod :: (p -> p) -> Dd s -> Dd s

instance (
    OverMod' p ps
  ) => OverMod p ('DdK sel ps a s0) where
    overMod f (Dd sel ps s) =
      Dd sel (overMod' f ps) s

class CMapMod c p0 p p1 s0 s1 | s0 p0 p1 -> p s1 where
  cmapMod :: p0 -> (c p p1 => p -> p1) -> Dd s0 -> Dd s1

instance (
    CMapMod' c p0 p p1 ps0 ps1
  ) => CMapMod c p0 p p1 ('DdK sel ps0 a s0) ('DdK sel ps1 a s0) where
  cmapMod p0 f (Dd sel ps0 s) =
    Dd sel (cmapMod' @c @_ @_ @p1 p0 f ps0) s

-- TODO this appends the mod if it is missing, while it should prepend it to preserve the order of effects.
amendMod ::
  MapMod p s0 s1 =>
  p ->
  Dd s0 ->
  Dd s1
amendMod p =
  mapMod p id

setMod ::
  MapMod p s0 s1 =>
  p ->
  Dd s0 ->
  Dd s1
setMod p =
  mapMod p (const p)

type OptMod :: Type -> [Type] -> Type -> Constraint
class OptMod p ps res | ps p -> res where
  optMod :: Mods ps -> res

instance OptMod p '[] () where
  optMod (Mods Nil) = ()

instance OptMod p (p : ps) p where
  optMod (Mods (I p :* _)) = p

instance {-# overlappable #-} (
    OptMod p ps p1
  ) => OptMod p (p0 : ps) p1 where
    optMod (Mods (_ :* ps)) = optMod @p (Mods ps)
