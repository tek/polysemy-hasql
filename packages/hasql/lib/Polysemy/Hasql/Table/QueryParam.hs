module Polysemy.Hasql.Table.QueryParam where

import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Generics.SOP (unComp, All, Code, I(I), NP, NS, Top, hcpure, hpure, (:.:)(Comp))
import Hasql.Encoders (Params, Value, array, dimension, element, nonNullable, nullable, param)
import Prelude hiding (All, null)

import Polysemy.Db.Data.Column (Flatten)
import Polysemy.Db.SOP.Contravariant (sequenceContravariantNPF)
import Polysemy.Hasql.Table.ValueEncoder (ValueEncoder(valueEncoder))

class QueryParam a where
  queryParam :: Params a

value :: Value a -> Params a
value =
  param . nonNullable

instance ValueEncoder a => QueryParam [a] where
  queryParam =
    value $ array (dimension foldl' (element (nonNullable valueEncoder)))
  {-# inline queryParam #-}

instance ValueEncoder a => QueryParam (Vector a) where
  queryParam =
    value $ array (dimension Vector.foldl' (element (nonNullable valueEncoder)))
  {-# inline queryParam #-}

instance ValueEncoder a => QueryParam (NonEmpty a) where
  queryParam =
    value $ array (dimension foldl' (element (nonNullable valueEncoder)))
  {-# inline queryParam #-}

instance ValueEncoder a => QueryParam (Maybe a) where
  queryParam =
    param $ nullable valueEncoder
  {-# inline queryParam #-}

instance {-# overlappable #-} ValueEncoder a => QueryParam a where
  queryParam =
    value valueEncoder
  {-# inline queryParam #-}

maybeParam ::
  ∀ a .
  ValueEncoder a =>
  (Params :.: Maybe) a
maybeParam =
  Comp (param (nullable (valueEncoder @a)))

maybeRow ::
  ∀ as .
  All ValueEncoder as =>
  Params (NP (I :.: Maybe) as)
maybeRow =
  sequenceContravariantNPF ps
  where
    ps :: NP (Params :.: Maybe) as
    ps =
      hcpure (Proxy @ValueEncoder) maybeParam

nothingNP ::
  All Top as =>
  NP (I :.: Maybe) as
nothingNP =
  hpure (Comp (I Nothing))

nulls ::
  ∀ (as :: [*]) a .
  All ValueEncoder as =>
  Params a
nulls =
  contramap (const (nothingNP @as)) maybeRow

class Null (rep :: [*]) a (ds :: [*]) where
  null :: Params ret

instance Null rep a '[] where
  null =
    mempty

instance {-# overlappable #-} (
    Null reps a ds,
    ValueEncoder d
  ) => Null (rep : reps) a (d : ds) where
  null =
    contramap (const Nothing) (unComp (maybeParam @d)) <> null @reps @a @ds

instance (
    Code rep ~ '[rSub],
    Code d ~ '[dSub],
    Null rSub a dSub,
    Null reps a ds
  ) => Null (Flatten rep : reps) a (d : ds) where
  null =
    null @rSub @a @dSub <> null @reps @a @ds

class Null2 (reps :: [[*]]) a (ass :: [[*]]) where
  null2 :: Params a

instance Null2 reps a '[] where
  null2 =
    mempty

instance (
    Null2 reps a ass,
    Null rep a as
  ) => Null2 (rep : reps) a (as : ass) where
  null2 =
    null @rep @a @as <> null2 @reps @a @ass

nullsSum ::
  ∀ (reps :: [*]) (as :: [*]) (ass :: [[*]]) .
  Null reps (NS (NP I) ass) as =>
  Params (NS (NP I) ass)
nullsSum =
  null @reps @(NS (NP I) ass) @as

allNulls ::
  ∀ (reps :: [[*]]) (as :: [*]) (ass :: [[*]]) .
  Null2 reps (NP I as) ass =>
  Params (NP I as)
allNulls =
  null2 @reps @(NP I as) @ass
