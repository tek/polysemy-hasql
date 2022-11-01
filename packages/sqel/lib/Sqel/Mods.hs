module Sqel.Mods where

import qualified Data.Aeson as Aeson
import Generics.SOP (I (I), NP (Nil, (:*)))
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

import Sqel.Data.Codec (Codec (Codec))
import Sqel.Data.Mods (Mods (Mods))
import Sqel.Data.PgType (PgPrimName)
import Sqel.Data.Sql (sql)
import Sqel.Sql.Prepared (dollar)
import Sqel.Sql.Select (FragType (Where), SelectAtom (SelectAtom))

defaultWhere :: SelectAtom
defaultWhere =
  SelectAtom Where (\ sel i -> [sql|##{sel} = #{dollar i}|])

data DefaultParam =
  DefaultParam
  deriving stock (Eq, Show, Generic)

jsonEncoder ::
  ToJSON a =>
  Encoders.Value a
jsonEncoder =
  toStrict . Aeson.encode >$< Encoders.jsonBytes

jsonDecoder ::
  FromJSON a =>
  Decoders.Value a
jsonDecoder =
  Decoders.jsonBytes (first toText . Aeson.eitherDecodeStrict')

data PrimCodec f a =
  PrimCodec (f a)

type PrimValueCodec a =
  PrimCodec (Codec Encoders.Value Decoders.Value) a

type PrimValueEncoder a =
  PrimCodec Encoders.Value a

primJsonValue ::
  ToJSON a =>
  FromJSON a =>
  Mods [PgPrimName, PrimValueCodec a]
primJsonValue =
  Mods (I "json" :* I (PrimCodec (Codec jsonEncoder jsonDecoder)) :* Nil)
