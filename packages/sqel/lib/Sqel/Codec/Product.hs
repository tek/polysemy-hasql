module Sqel.Codec.Product where

import Generics.SOP (
  All,
  I,
  K (K),
  NP,
  NS (Z),
  Projection,
  SOP (SOP),
  Top,
  hcmap,
  hcollapse,
  hsequence,
  hzipWith,
  projections,
  unI,
  unSOP,
  unZ,
  type  (-.->) (Fn),
  )
import Generics.SOP.GGP (gfrom, gto)
import Lens.Micro.Extras (view)

import qualified Sqel.Data.Codec as Codec
import Sqel.Data.Codec (Codec (Codec), Decoder, Encoder, FullCodec)
import Sqel.SOP.Constraint (ConstructProd, ReifyProd)

prodParams ::
  ∀ as b .
  Contravariant b =>
  (∀ x . Monoid (b x)) =>
  All Top as =>
  NP b as ->
  b (NP I as)
prodParams np =
  mconcat (hcollapse qps)
  where
    qps :: NP (K (b (NP I as))) as
    qps =
      hzipWith qp np (projections :: NP (Projection I as) as)
    {-# inline qps #-}
    qp :: ∀ a . b a -> Projection I as a -> K (b (NP I as)) a
    qp par (Fn proj) =
      K (contramap (unI . proj . K) par)
    {-# inline qp #-}
{-# inline prodParams #-}

type GetEncoder :: (Type -> Type) -> Type -> Constraint
class GetEncoder b a where
  getEncoder :: b a -> Encoder a

instance GetEncoder FullCodec a where
  getEncoder = view #encoder

instance GetEncoder Encoder a where
  getEncoder = id

type GetDecoder :: (Type -> Type) -> Type -> Constraint
class GetDecoder b a where
  getDecoder :: b a -> Decoder a

instance GetDecoder FullCodec a where
  getDecoder = view #decoder

type ProdEncoder :: (Type -> Type) -> Type -> [Type] -> Constraint
class ProdEncoder b a as | a -> as where
  prodEncoder :: NP b as -> Encoder a

instance (
    ConstructProd a as,
    All (GetEncoder b) as
  ) => ProdEncoder b a as where
    prodEncoder np = unZ . unSOP . gfrom >$< prodParams (hcmap (Proxy @(GetEncoder b)) getEncoder np)

type ProdDecoder :: (Type -> Type) -> Type -> [Type] -> Constraint
class ProdDecoder b a as | a -> as where
  prodDecoder :: NP b as -> Decoder a

instance (
    ReifyProd a as,
    All (GetDecoder b) as
  ) => ProdDecoder b a as where
    prodDecoder np = gto . SOP . Z <$> hsequence (hcmap (Proxy @(GetDecoder b)) getDecoder np)

type ProdCodec :: (Type -> Type) -> Type -> [Type] -> Constraint
class ProdCodec b a as | a -> as where
  prodCodec :: NP b as -> b a

instance (
    ProdDecoder FullCodec a as,
    ProdEncoder FullCodec a as
  ) => ProdCodec FullCodec a as where
    prodCodec np =
      Codec {
        decoder = prodDecoder np,
        encoder = prodEncoder np
      }

instance (
    ProdEncoder Encoder a as
  ) => ProdCodec Encoder a as where
    prodCodec = prodEncoder
