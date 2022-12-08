module Sqel.Column where

import Generics.SOP (I, NP)
import qualified Hasql.Decoders as Hasql
import Hasql.Decoders (Row, custom)

import Sqel.Data.ColumnOptions (ColumnOptions)
import Sqel.Data.Dd (Dd (Dd), DdK (DdK), DdStruct (DdPrim), Struct (Prim))
import Sqel.Data.Mods (AddMod (addMod), MapMod (mapMod), Mods)
import Sqel.Data.Sel (Sel (SelAuto, SelSymbol), mkSel)
import Sqel.Names.Rename (Rename, rename)
import Sqel.Names.Set (SetName)

data Nullable =
  Nullable
  deriving stock (Show)

newtype Effects es =
  Effects { unEffects :: NP I es }
  deriving stock (Generic)

class AddEffect e e0 e1 | e e0 -> e1 where
  addEffect :: e -> e0 -> e1

ignoreDecoder :: Row (Maybe a)
ignoreDecoder =
  join <$> Hasql.column (Hasql.nullable (custom \ _ _ -> pure Nothing))

class MkColumn a p0 sel p | p0 -> sel p where
  column :: p0 -> Dd ('DdK sel p a 'Prim)

instance MkColumn a (Mods p) 'SelAuto (Mods p) where
  column p = Dd mkSel p DdPrim

instance MkColumn a (Dd ('DdK sel p a 'Prim)) sel p where
  column = id

columnAs ::
  ∀ name a p0 sel p .
  KnownSymbol name =>
  MkColumn a p0 sel p =>
  p0 ->
  Dd ('DdK ('SelSymbol name) p a 'Prim)
columnAs =
  rename . column @a

class WithOption s0 s1 | s0 -> s1 where
  withOption :: (ColumnOptions -> ColumnOptions) -> Dd s0 -> Dd s1

instance (
    MapMod ColumnOptions s0 s1
  ) => WithOption s0 s1 where
  withOption f =
    mapMod def f

pk ::
  WithOption s0 s1 =>
  Dd s0 ->
  Dd s1
pk =
  withOption (#primaryKey .~ True)

class MkNullable s0 s1 | s0 -> s1 where
  mkNullable :: Dd s0 -> Dd s1

instance MkNullable ('DdK sel p a 'Prim) ('DdK sel p (Maybe a) 'Prim) where
  mkNullable (Dd sel p s) = Dd sel p s

nullable ::
  ∀ s0 s1 s2 s3 .
  WithOption s0 s1 =>
  AddMod Nullable s1 s2 =>
  MkNullable s2 s3 =>
  Dd s0 ->
  Dd s3
nullable =
  mkNullable .
  addMod Nullable .
  withOption (#notNull .~ False)

nullableAs ::
  ∀ name s0 s1 s2 s3 .
  WithOption s0 s1 =>
  AddMod Nullable s1 s2 =>
  MkNullable s2 s3 =>
  Rename s3 (SetName s3 name) =>
  Dd s0 ->
  Dd (SetName s3 name)
nullableAs =
  rename . nullable
