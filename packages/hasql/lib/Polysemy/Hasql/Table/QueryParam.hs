module Polysemy.Hasql.Table.QueryParam where

import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Generics.SOP (All, I(I), NP, NS, Top, hcpure, hpure, (:.:)(Comp))
import Hasql.Encoders (Params, Value, array, dimension, element, nonNullable, nullable, param)
import Prelude hiding (All)

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

nullsSum ::
  ∀ (as :: [*]) (ass :: [[*]]) .
  All ValueEncoder as =>
  Params (NS (NP I) ass)
nullsSum =
  nulls @as

class Null2 a (ass :: [[*]]) where
  null2 :: Params a

instance Null2 a '[] where
  null2 =
    mempty

instance (
    Null2 a ass,
    All ValueEncoder as
  ) => Null2 a (as : ass) where
  null2 =
    nulls @as <> null2 @a @ass

allNulls ::
  ∀ (as :: [*]) (ass :: [[*]]) .
  Null2 (NP I as) ass =>
  Params (NP I as)
allNulls =
  null2 @(NP I as) @ass
