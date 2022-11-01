module Sqel.Data.Mods where

import Exon (exon)
import Generics.SOP (All, Compose, I (I), NP (Nil, (:*)))
import Prelude hiding (Compose)
import Text.Show (showParen, showsPrec)

import Sqel.Data.Dd (Dd (Dd), DdK (DdK), (:>) ((:>)))

newtype Mods ps = Mods { unMods :: NP I ps }

type NoMods = Mods '[]

instance (
    All (Compose Show I) ps
  ) => Show (Mods ps) where
  showsPrec d (Mods ps) =
    showParen (d > 10) [exon|Mods #{showsPrec 11 ps}|]

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

instance {-# overlappable #-} (
    GetMod c p ps
  ) => GetMod c p (a' : ps) where
    getMod f (Mods (_ :* ps)) =
      getMod @c f (Mods ps)

class MapModDd p s0 s1 | p s0 -> s1 where
  mapModDd :: p -> (p -> p) -> Dd s0 -> Dd s1

instance (
    MapMod' p ps0 ps1
  ) => MapModDd p ('DdK sel (Mods ps0) a s0) ('DdK sel (Mods ps1) a s0) where
    mapModDd p f (Dd sel ps0 s) =
      Dd sel (mapMod' p f ps0) s

class CMapMod c p0 p p1 s0 s1 | s0 p0 p1 -> p s1 where
  cmapMod :: p0 -> (c p p1 => p -> p1) -> Dd s0 -> Dd s1

instance (
    CMapMod' c p0 p p1 ps0 ps1
  ) => CMapMod c p0 p p1 ('DdK sel (Mods ps0) a s0) ('DdK sel (Mods ps1) a s0) where
  cmapMod p0 f (Dd sel ps0 s) =
    Dd sel (cmapMod' @c @_ @_ @p1 p0 f ps0) s

amendMod ::
  MapModDd p s0 s1 =>
  p ->
  Dd s0 ->
  Dd s1
amendMod p =
  mapModDd p id

setMod ::
  MapModDd p s0 s1 =>
  p ->
  Dd s0 ->
  Dd s1
setMod p =
  mapModDd p (const p)

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
