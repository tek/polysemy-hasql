module Sqel.ReifyCodec where

import Generics.SOP (AllZip, I (I), NP ((:*)), htrans)
import qualified Hasql.Encoders as Encoders

import Sqel.Codec (
  ColumnDecoder (columnDecoder, columnDecoderNullable),
  ColumnEncoder (columnEncoder, columnEncoderIgnore, columnEncoderNullable),
  PrimColumn (primDecoder, primEncoder),
  fullPrimCodec,
  )
import Sqel.Codec.PrimDecoder (ArrayDecoder (arrayDecoder), enumDecoder, readDecoder)
import Sqel.Codec.PrimEncoder (arrayEncoder)
import Sqel.Codec.Product (ProdCodec (prodCodec))
import Sqel.Codec.Sum (ConCodec (conCodec), SumCodec (sumCodec), ignoreEncoder)
import Sqel.Column (Nullable (Nullable), ignoreDecoder)
import qualified Sqel.Data.Codec as Codec
import Sqel.Data.Codec (Codec (Codec), Decoder (Decoder), Encoder (Encoder), FullCodec, ValueCodec)
import Sqel.Data.Dd (
  Comp (Prod, Sum),
  CompInc,
  ConCol,
  Dd (Dd),
  DdK (DdK),
  DdStruct (DdComp, DdPrim),
  ProdType (Con, Reg),
  Struct (Comp, Prim),
  )
import Sqel.Data.Mods (ArrayColumn (ArrayColumn), EnumColumn (EnumColumn), Mods (Mods), ReadShowColumn (ReadShowColumn))
import Sqel.Data.Sel (Sel (SelSymbol))
import Sqel.Mods (PrimCodec (PrimCodec), PrimValueCodec, PrimValueEncoder)
import Sqel.SOP.Enum (EnumTable)

type CompCodec :: Comp -> CompInc -> Type -> (Type -> Type) -> [Type] -> Constraint
class CompCodec c i a b as | a -> as where
  compCodec :: NP b as -> b a

instance (
    ProdCodec b a as
  ) => CompCodec ('Prod 'Reg) i a b as where
    compCodec = prodCodec

instance (
    ConCodec b as
  ) => CompCodec ('Prod ('Con as)) i (ConCol as) b as where
    compCodec = conCodec

instance (
    SumCodec b a as
  ) => CompCodec 'Sum i a b as where
    compCodec = sumCodec

class DefaultPrimCodec b a where
  defaultPrimCodec :: b a

instance (
    PrimColumn a
  ) => DefaultPrimCodec FullCodec a where
    defaultPrimCodec =
      Codec {
        encoder = Encoder (columnEncoder encoder) (columnEncoderIgnore encoder),
        decoder = Decoder (columnDecoder decoder) (void ignoreDecoder)
      }
      where
        encoder = primEncoder
        decoder = primDecoder

instance (
    PrimColumn a
  ) => DefaultPrimCodec ValueCodec a where
    defaultPrimCodec =
      Codec {
        encoder = primEncoder,
        decoder = primDecoder
      }

instance (
    PrimColumn a
  ) => DefaultPrimCodec Encoder a where
    defaultPrimCodec =
      Encoder (columnEncoder encoder) (columnEncoderIgnore encoder)
      where
        encoder = primEncoder

instance (
    PrimColumn a
  ) => DefaultPrimCodec Encoders.Value a where
    defaultPrimCodec = primEncoder

type DefaultCompCodec :: Comp -> CompInc -> (Type -> Type) -> Type -> [Type] -> Constraint
class DefaultCompCodec c i b a as | a -> as where
  defaultCompCodec :: NP b as -> b a

instance (
    CompCodec c i a FullCodec as
  ) => DefaultCompCodec c i FullCodec a as where
    defaultCompCodec = compCodec @c @i

instance (
    CompCodec c i a Encoder as
  ) => DefaultCompCodec c i Encoder a as where
    defaultCompCodec = compCodec @c @i

class ReifyPrimCodec b ps a where
  reifyPrimCodec :: NP I ps -> b a

instance {-# overlappable #-} (
    ReifyPrimCodec b ps a
  ) => ReifyPrimCodec b (p : ps) a where
  reifyPrimCodec (_ :* ps) =
    reifyPrimCodec ps

instance ReifyPrimCodec ValueCodec (PrimValueCodec a : ps) a where
  reifyPrimCodec (I (PrimCodec c) :* _) = c

instance ReifyPrimCodec FullCodec (PrimValueCodec a : ps) a where
  reifyPrimCodec (I (PrimCodec (Codec encoder decoder)) :* _) =
    Codec {
      encoder = Encoder (columnEncoder encoder) (columnEncoderIgnore encoder),
      decoder = Decoder (columnDecoder decoder) (void ignoreDecoder)
    }

instance (
    ReifyPrimCodec ValueCodec ps a
  ) => ReifyPrimCodec FullCodec (Nullable : ps) (Maybe a) where
    reifyPrimCodec (I Nullable :* ps) =
      Codec {
        encoder = Encoder (columnEncoderNullable encoder) (ignoreEncoder encoder),
        decoder = Decoder (columnDecoderNullable decoder) (void ignoreDecoder)
      }
      where
        Codec encoder decoder = reifyPrimCodec @ValueCodec ps

instance ReifyPrimCodec Encoders.Value (PrimValueEncoder a : ps) a where
  reifyPrimCodec (I (PrimCodec e) :* _) = e

instance ReifyPrimCodec Encoder (PrimValueEncoder a : ps) a where
  reifyPrimCodec (I (PrimCodec encoder) :* _) =
    Encoder (columnEncoder encoder) (columnEncoderIgnore encoder)

-- TODO this could also produce NullableOrNot
instance (
    ReifyPrimCodec Encoders.Value ps a
  ) => ReifyPrimCodec Encoder (Nullable : ps) (Maybe a) where
    reifyPrimCodec (I Nullable :* ps) =
      Encoder (columnEncoderNullable encoder) (ignoreEncoder encoder)
      where
        encoder = reifyPrimCodec @Encoders.Value ps

instance (
    Show a,
    EnumTable a
  ) => ReifyPrimCodec FullCodec (EnumColumn : ps) a where
  reifyPrimCodec (I EnumColumn :* _) =
    fullPrimCodec (Encoders.enum show) enumDecoder

instance (
    Show a,
    EnumTable a
  ) => ReifyPrimCodec ValueCodec (EnumColumn : ps) a where
  reifyPrimCodec (I EnumColumn :* _) =
    Codec (Encoders.enum show) enumDecoder

instance (
    Show a,
    Read a
  ) => ReifyPrimCodec FullCodec (ReadShowColumn : ps) a where
  reifyPrimCodec (I ReadShowColumn :* _) =
    fullPrimCodec (Encoders.enum show) readDecoder

instance (
    Show a,
    Read a
  ) => ReifyPrimCodec ValueCodec (ReadShowColumn : ps) a where
  reifyPrimCodec (I ReadShowColumn :* _) =
    Codec (Encoders.enum show) readDecoder

instance (
    ReifyPrimCodec ValueCodec ps a,
    Foldable f,
    ArrayDecoder f a
  ) => ReifyPrimCodec ValueCodec (ArrayColumn f : ps) (f a) where
  reifyPrimCodec (I ArrayColumn :* ps) =
    Codec (arrayEncoder encoder) (arrayDecoder decoder)
      where
        Codec encoder decoder = reifyPrimCodec @ValueCodec ps

instance (
    ReifyPrimCodec ValueCodec ps a,
    Foldable f,
    ArrayDecoder f a
  ) => ReifyPrimCodec FullCodec (ArrayColumn f : ps) (f a) where
  reifyPrimCodec (I ArrayColumn :* ps) =
    fullPrimCodec (arrayEncoder encoder) (arrayDecoder decoder)
      where
        Codec encoder decoder = reifyPrimCodec @ValueCodec ps

instance (
    DefaultPrimCodec b a
  ) => ReifyPrimCodec b '[] a where
    reifyPrimCodec _ = defaultPrimCodec

class ReifyCompCodec b c i ps as a where
  reifyCompCodec :: NP I ps -> NP b as -> b a

instance (
    DefaultCompCodec c i b a as
  ) => ReifyCompCodec b c i ps as a where
    reifyCompCodec _ sub =
      defaultCompCodec @c @i sub

class ReifyCodec b s a | s -> a where
  reifyCodec :: Dd s -> b a

instance (
    ReifyPrimCodec b ps a
  ) => ReifyCodec b ('DdK sel ps a 'Prim) a where
    reifyCodec (Dd _ (Mods ps) DdPrim) =
      reifyPrimCodec @b ps

instance (
    AllZip (ReifyCodec b) sub as,
    ReifyCompCodec b c i ps as a
  ) => ReifyCodec b ('DdK sel ps a ('Comp ('SelSymbol tname) c i sub)) a where
    reifyCodec (Dd _ (Mods ps) (DdComp _ _ _ sub)) =
      reifyCompCodec @b @c @i @ps @as ps (htrans (Proxy @(ReifyCodec b)) reifyCodec sub)
