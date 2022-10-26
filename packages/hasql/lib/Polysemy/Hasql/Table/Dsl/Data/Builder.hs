module Polysemy.Hasql.Table.Dsl.Data.Builder where

import Hasql.Decoders (Row)
import Hasql.Encoders (Params)

import Polysemy.Hasql.Table.Dsl.Data.PgColumn (PgField)

data Builder e d a =
  Builder {
    column :: PgField a,
    encoder :: e a,
    decoder :: d a
  }
  deriving stock (Generic)

data Encoder a =
  Encoder {
    encodeValue :: Params a,
    encodeNulls :: Params ()
  }
  deriving stock (Generic)

instance Semigroup (Encoder a) where
  Encoder vl nl <> Encoder vr nr =
    Encoder (vl <> vr) (nl <> nr)

instance Monoid (Encoder a) where
  mempty =
    Encoder mempty mempty

instance Contravariant Encoder where
  contramap f Encoder {..} =
    Encoder (f >$< encodeValue) encodeNulls

data Decoder a =
  Decoder {
    decodeValue :: Row a,
    decodeNulls :: Row ()
  }
  deriving stock (Generic)

instance Functor Decoder where
  fmap f Decoder {..} =
    Decoder (f <$> decodeValue) decodeNulls

instance Applicative Decoder where
  pure a =
    Decoder (pure a) (pure ())

  liftA2 f (Decoder vl nl) (Decoder vr nr) =
    Decoder (liftA2 f vl vr) (nl *> nr)
