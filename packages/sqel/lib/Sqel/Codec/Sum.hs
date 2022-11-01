module Sqel.Codec.Sum where

import Data.Functor.Contravariant.Divisible (choose)
import Data.Functor.Invariant (Invariant (invmap))
import Exon (exon)
import Generics.SOP (
  All2,
  HIndex (hindex),
  I,
  NP (Nil, (:*)),
  NS (S, Z),
  SListI,
  SListI2,
  SOP (SOP),
  Top,
  hcfoldMap,
  hctraverse_,
  hmap,
  hsequence,
  unSOP,
  )
import Generics.SOP.GGP (gfrom, gto)
import Hasql.Decoders (Row)
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Encoders as Encoder
import Hasql.Encoders (Params)
import Lens.Micro.Extras (view)

import Sqel.Codec.Product (prodParams)
import qualified Sqel.Data.Codec as Codec
import Sqel.Data.Codec (Codec (Codec), Decoder (Decoder), Encoder (Encoder), FullCodec)
import Sqel.Data.Dd (ConCol (ConCol, unConCol))
import Sqel.SOP.Constraint (ConstructSOP, ReifySOP)

unconsNS ::
  NS (NP I) (ds : dss) ->
  Either (NP I ds) (NS (NP I) dss)
unconsNS = \case
  Z x -> Left x
  S x -> Right x

newtype ConB b as =
  ConB { unConB :: b (NP I as) }

readNull ::
  ∀ as .
  Decoder (NP I as) ->
  Row ()
readNull rs =
  rs ^. #decodeNulls

readNulls ::
  ∀ ass .
  SListI2 ass =>
  NP (ConB Decoder) ass ->
  Row ()
readNulls cons =
  hctraverse_ (Proxy @SListI) (readNull . unConB) cons

sumRows ::
  All2 Top ass =>
  NP (ConB Decoder) ass ->
  Int ->
  Row (NS (NP I) ass)
sumRows (ConB con :* cons) 0 =
  Z <$> (con ^. #decodeValue) <* readNulls cons
sumRows (ConB con :* cons) index = do
  readNull con
  S <$> sumRows cons (index - 1)
sumRows Nil index =
  fail [exon|invalid index into sum type in database: #{show index}|]

ignoreEncoder :: Encoder.Value a -> Params b
ignoreEncoder v =
  const Nothing >$< Encoders.param (Encoders.nullable v)

writeNull ::
  ∀ a as .
  ConB Encoder as ->
  Params a
writeNull (ConB enc) =
  contramap unit (enc ^. #encodeNulls)

writeNulls ::
  ∀ a ass .
  SListI2 ass =>
  NP (ConB Encoder) ass ->
  Params a
writeNulls =
  hcfoldMap (Proxy @SListI) writeNull

sumParams ::
  All2 Top ass =>
  NP (ConB Encoder) ass ->
  Params (NS (NP I) ass)
sumParams = \case
  con :* cons ->
    choose unconsNS inhabited uninhabited
    where
      inhabited = (unConB con) ^. #encodeValue <> writeNulls cons
      uninhabited = writeNull con <> sumParams cons
  Nil ->
    mempty

type WrapConB :: (Type -> Type) -> [[Type]] -> [Type] -> Constraint
class WrapConB b ass as | ass -> as where
  wrapConB :: NP b as -> NP (ConB b) ass

instance WrapConB b '[] '[] where
  wrapConB Nil = Nil

instance (
    Invariant b,
    WrapConB b ass as
  ) => WrapConB b (as' : ass) (ConCol as' : as) where
    wrapConB (b :* bs) =
      ConB (invmap unConCol ConCol b) :* wrapConB bs

encodeValue ::
  ConstructSOP a ass =>
  Encoder Int ->
  NP (ConB Encoder) ass ->
  Params a
encodeValue (Encoder indexParams _) wrapped =
  unSOP . gfrom >$< (indexEncoder <> sumParams wrapped)
  where
    indexEncoder = hindex >$< indexParams

type SumCodec :: (Type -> Type) -> Type -> [Type] -> Constraint
class SumCodec b a as | a -> as where
  sumCodec :: NP b as -> b a

-- TODO add null builders
instance (
    ReifySOP a ass,
    ConstructSOP a ass,
    WrapConB FullCodec ass as
  ) => SumCodec FullCodec a (Int : as) where
    sumCodec (Codec index (Decoder indexRow _) :* conCodecs) =
      Codec {
        decoder = Decoder decodeValue unit,
        encoder = Encoder (encodeValue index (hmap (ConB . view #encoder . unConB) wrapped)) mempty
      }
      where
        decodeValue =
          gto . SOP <$> (sumRows decs =<< indexRow)
        decs =
          hmap (ConB . view #decoder . unConB) wrapped
        wrapped =
          wrapConB conCodecs

instance (
    ConstructSOP a ass,
    WrapConB Encoder ass as
  ) => SumCodec Encoder a (Int : as) where
    sumCodec (index :* conCodecs) =
      Encoder (encodeValue index wrapped) mempty
      where
        wrapped = wrapConB conCodecs

type ConCodec :: (Type -> Type) -> [Type] -> Constraint
class ConCodec b as where
  conCodec :: NP b as -> b (ConCol as)

instance SListI as => ConCodec FullCodec as where
  conCodec np =
    Codec {
      decoder = ConCol <$> hsequence (hmap (view #decoder) np),
      encoder = unConCol >$< prodParams (hmap (view #encoder) np)
    }

instance SListI as => ConCodec Encoder as where
    conCodec np = unConCol >$< prodParams np
