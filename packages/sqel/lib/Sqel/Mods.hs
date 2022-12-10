module Sqel.Mods where

import qualified Data.Aeson as Aeson
import Generics.SOP (I (I), NP (Nil, (:*)))
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Text.Show (show)

import Sqel.Data.Codec (Codec (Codec))
import Sqel.Data.Mods (
  EnumColumn (EnumColumn),
  Mods (Mods),
  ReadShowColumn (ReadShowColumn),
  SetTableName (SetTableName),
  )
import Sqel.Data.PgType (PgPrimName)
import Sqel.Data.PgTypeName (PgTableName)
import Sqel.Data.Sql (sql)
import Sqel.Sql.Prepared (dollar)
import Sqel.Sql.Select (FragType (Where), SelectAtom (SelectAtom))

defaultWhere :: SelectAtom
defaultWhere =
  SelectAtom Where (\ sel i -> [sql|##{sel} = #{dollar i}|])

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

instance Show (PrimCodec f a) where
  show _ =
    "PrimCodec"

type PrimValueCodec a =
  PrimCodec (Codec Encoders.Value Decoders.Value) a

type PrimValueEncoder a =
  PrimCodec Encoders.Value a

primJsonMods ::
  ToJSON a =>
  FromJSON a =>
  Mods [PgPrimName, PrimValueCodec a]
primJsonMods =
  Mods (I "json" :* I (PrimCodec (Codec jsonEncoder jsonDecoder)) :* Nil)

-- TODO change to "enum", create the type just like other composites
primEnumMods :: Mods [PgPrimName, EnumColumn]
primEnumMods =
  Mods (I "text" :* I EnumColumn :* Nil)

primReadShowMods :: Mods [PgPrimName, ReadShowColumn]
primReadShowMods =
  Mods (I "text" :* I ReadShowColumn :* Nil)

tableNameMods :: PgTableName -> Mods '[SetTableName]
tableNameMods n =
  Mods (I (SetTableName n) :* Nil)
