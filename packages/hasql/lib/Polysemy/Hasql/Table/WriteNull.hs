module Polysemy.Hasql.Table.WriteNull where

import Hasql.Encoders (Params)
import Polysemy.Db.SOP.Constraint (ProductCoded)

import Polysemy.Hasql.Table.QueryParam (QueryParam, queryParam)
import Polysemy.Hasql.Column.Data.Effect (Tc)
import qualified Polysemy.Hasql.Kind.Data.DbType as Kind

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

class NullColumns (ds :: [*]) (cs :: [Kind.Column]) where
  nullColumns :: Params d

instance NullColumns '[] '[] where
  nullColumns =
    mempty

type family WithMaybe (d :: *) (init :: [*]) (effs :: [*]) :: [*] where
  WithMaybe d init '[] = Tc Maybe d : init
  WithMaybe _ init (Tc Maybe _ : _) = init
  WithMaybe d init (_ : effs) = WithMaybe d init effs

instance {-# overlappable #-} (
    maybeEff ~ WithMaybe d eff eff,
    NullColumns ds cs,
    NullParam d ~ Maybe a,
    NullParam a ~ Maybe a,
    QueryParam maybeEff (NullParam d)
  ) => NullColumns (d : ds) ('Kind.Column n eff ('Kind.Prim d) : cs) where
  nullColumns =
    contramap (const Nothing) (maybeParam @maybeEff @a) <> nullColumns @ds @cs

-- TODO sum with Flatten
instance (
    ProductCoded d dSub,
    NullColumns dSub c,
    NullColumns ds cs
  ) => NullColumns (d : ds) ('Kind.Column n eff ('Kind.Prod d c) : cs) where
  nullColumns =
    nullColumns @dSub @c <> nullColumns @ds @cs

class WriteNullCon (ds :: [*]) (c :: Kind.Column) where
  writeNullCon :: Params d

instance (
    NullColumns '[d] '[ 'Kind.Column n eff ('Kind.Prim d)]
  ) => WriteNullCon '[d] ('Kind.Column n eff ('Kind.Prim d)) where
  writeNullCon =
    nullColumns @'[d] @'[ 'Kind.Column n eff ('Kind.Prim d)]

instance (
  NullColumns ds cs
  ) => WriteNullCon ds ('Kind.Column n eff ('Kind.Prod d cs)) where
  writeNullCon =
    nullColumns @ds @cs

class WriteNullCons (dss :: [[*]]) (cs :: [Kind.Column]) where
  writeNullCons :: Params a

instance WriteNullCons '[] '[] where
  writeNullCons =
    mempty

instance (
    WriteNullCon ds c,
    WriteNullCons dss cs
  ) => WriteNullCons (ds : dss) (c : cs) where
  writeNullCons =
    writeNullCon @ds @c <> writeNullCons @dss @cs
