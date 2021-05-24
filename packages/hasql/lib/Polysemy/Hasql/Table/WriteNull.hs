module Polysemy.Hasql.Table.WriteNull where

import Hasql.Encoders (Params)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree.Data.Effect (Tycon)

import Polysemy.Hasql.Table.QueryParam (QueryParam, queryParam)

type family NullParam a :: * where
  NullParam (Maybe a) = Maybe a
  NullParam a = Maybe a

maybeParam ::
  ∀ eff a .
  NullParam a ~ Maybe a =>
  QueryParam eff (NullParam a) =>
  Params (Maybe a)
maybeParam =
  queryParam @eff @(NullParam a)

class NullColumns (cs :: [Kind.Tree]) where
  nullColumns :: Params d

instance NullColumns '[] where
  nullColumns =
    mempty

type family WithMaybe (d :: *) (init :: [*]) (effs :: [*]) :: [*] where
  WithMaybe d init '[] = Tycon Maybe d : init
  WithMaybe _ init (Tycon Maybe _ : _) = init
  WithMaybe d init (_ : effs) = WithMaybe d init effs

instance {-# overlappable #-} (
    maybeEff ~ WithMaybe d eff eff,
    NullColumns cs,
    NullParam d ~ Maybe a,
    NullParam a ~ Maybe a,
    QueryParam maybeEff (NullParam d)
  ) => NullColumns ('Kind.Tree n eff ('Kind.Prim d) : cs) where
  nullColumns =
    contramap (const Nothing) (maybeParam @maybeEff @a) <> nullColumns @cs

-- TODO sum with Flatten
instance (
    NullColumns c,
    NullColumns cs
  ) => NullColumns ('Kind.Tree n eff ('Kind.Prod d c) : cs) where
  nullColumns =
    nullColumns @c <> nullColumns @cs

class WriteNullCon (c :: Kind.Con) where
  writeNullCon :: Params d

instance (
    NullColumns '[tree]
  ) => WriteNullCon ('Kind.ConUna num n tree) where
  writeNullCon =
    nullColumns @'[tree]

instance (
  NullColumns cs
  ) => WriteNullCon ('Kind.Con num n cs) where
  writeNullCon =
    nullColumns @cs

class WriteNullCons (cs :: [Kind.Con]) where
  writeNullCons :: Params a

instance WriteNullCons '[] where
  writeNullCons =
    mempty

instance (
    WriteNullCon c,
    WriteNullCons cs
  ) => WriteNullCons (c : cs) where
  writeNullCons =
    writeNullCon @c <> writeNullCons @cs
