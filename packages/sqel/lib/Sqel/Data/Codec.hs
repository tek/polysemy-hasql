module Sqel.Data.Codec where

import Data.Functor.Invariant (Invariant (invmap), invmapContravariant)
import qualified Hasql.Decoders as Decoders
import Hasql.Decoders (Row)
import qualified Hasql.Encoders as Encoders
import Hasql.Encoders (Params)

import Sqel.SOP.Constraint (symbolText)

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

instance Invariant Encoder where
  invmap = invmapContravariant

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

data Codec e d a =
  Codec {
    encoder :: e a,
    decoder :: d a
  }
  deriving stock (Generic)

type ValueCodec =
  Codec Encoders.Value Decoders.Value

instance (
    Contravariant e,
    Functor d
  ) => Invariant (Codec e d) where
    invmap f c Codec {..} =
      Codec {
        encoder = c >$< encoder,
        decoder = f <$> decoder
      }

type FullCodec =
  Codec Encoder Decoder

newtype ColumnName =
  ColumnName { unColumnName :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

symbolColumnName ::
  ∀ (name :: Symbol) .
  KnownSymbol name =>
  ColumnName
symbolColumnName =
  ColumnName (symbolText @name)
