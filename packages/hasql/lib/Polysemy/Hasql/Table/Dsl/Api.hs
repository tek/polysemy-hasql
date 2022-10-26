module Polysemy.Hasql.Table.Dsl.Api (
  module Polysemy.Hasql.Table.Dsl.Api,
  prod,
  flatten,
  sum,
) where

import Generics.SOP (NP (Nil, (:*)))
import qualified Hasql.Decoders as Decoders
import Hasql.Decoders (Row)
import qualified Hasql.Encoders as Encoders
import Hasql.Encoders (Params)
import Polysemy.Db.Data.ColumnOptions (ColumnOptions)
import Polysemy.Db.Data.ColumnPrefix (ColumnPrefix, addPrefix, prefixed)
import Polysemy.Db.Data.Uid (Uid)
import Polysemy.Db.Text.DbIdentifier (dbIdentifierT)
import Prelude hiding (sum)

import Polysemy.Hasql.Data.DbType (
  Column (Column),
  DbType (Prim, Prod),
  Name (Name),
  TypeName (CompositeTypeName, PrimTypeName),
  textSelector,
  )
import Polysemy.Hasql.Table.Dsl.Codec (
  ColumnDecoder (columnDecoder, columnDecoderNullable),
  ColumnEncoder (columnEncoder, columnEncoderNullable),
  PrimColumn (pgType, primDecoder, primEncoder),
  )
import qualified Polysemy.Hasql.Table.Dsl.Data.Builder as Builder
import Polysemy.Hasql.Table.Dsl.Data.Builder (Builder (Builder), Decoder (Decoder), Encoder (Encoder))
import qualified Polysemy.Hasql.Table.Dsl.Data.PgColumn as PgColumn
import Polysemy.Hasql.Table.Dsl.Data.PgColumn (
  PgColumn (PgColumn),
  PgField (PgFieldColumn, PgFieldFlatten),
  PgPrimName (PgPrimName),
  PgProdName (PgProdName),
  PgType (PgTypePrim, PgTypeProd),
  addOptions,
  )
import Polysemy.Hasql.Table.Dsl.Product (ProdWith, flatten, prod)
import Polysemy.Hasql.Table.Dsl.Sum (ignoreEncoder, sum)
import Polysemy.Hasql.Table.ReadNull (ignoreDecoder)

typeColumn :: ColumnPrefix -> PgType a -> (TypeName, DbType)
typeColumn prefix = \case
  PgTypePrim (PgPrimName t) ->
    (PrimTypeName t, Prim)
  PgTypeProd (PgProdName t) fs ->
    (CompositeTypeName t, Prod (uncurry (toColumns prefix) =<< fs))

toColumn :: ColumnPrefix -> Text -> PgColumn a -> Column
toColumn prefix name PgColumn {..} =
  Column (Name (dbIdentifierT name)) (textSelector (prefixed name prefix)) tn options tpe
  where
    (tn, tpe) =
      typeColumn newPrefix colType
    newPrefix =
      addPrefix name prefix

toColumns :: ColumnPrefix -> Text -> PgField a -> [Column]
toColumns prefix _ (PgFieldFlatten _ cols) =
  uncurry (toColumns prefix) =<< cols
toColumns prefix name (PgFieldColumn col) =
  [toColumn prefix name col]

rootColumn :: PgField a -> PgColumn a
rootColumn = \case
  PgFieldColumn c -> c
  PgFieldFlatten n cols -> PgColumn (PgTypeProd n cols) mempty

pgColumn :: PrimColumn a => PgField a
pgColumn =
  PgFieldColumn (PgColumn pgType mempty)

data Table a =
  Table {
    structure :: PgColumn a,
    decoder :: Row a,
    encoder :: Params a
  }
  deriving stock (Generic)

table :: Builder Encoder Decoder a -> Table a
table Builder {column = (rootColumn -> structure), ..} =
  Table {encoder = encoder ^. #encodeValue, decoder = decoder ^. #decodeValue, ..}

primValue ::
  PrimColumn a =>
  Builder Encoders.Value Decoders.Value a
primValue =
  Builder pgColumn primEncoder primDecoder

maybeColumn ::
  ColumnEncoder e =>
  ColumnDecoder d =>
  ColumnOptions ->
  Builder e d a ->
  Builder Params Row (Maybe a)
maybeColumn opt Builder {column = col, ..} =
  Builder {
    encoder = columnEncoderNullable encoder,
    decoder = columnDecoderNullable decoder,
    column = coerce (addOptions (opt & #notNull .~ False) col)
  }

column :: ColumnOptions -> Builder Encoders.Value Decoders.Value a -> Builder Encoder Decoder a
column opt Builder {column = col, ..} =
  Builder {
    encoder = Encoder (columnEncoder encoder) (ignoreEncoder),
    decoder = Decoder (columnDecoder decoder) (void ignoreDecoder),
    column = addOptions (opt & #notNull .~ True) col
  }

prim ::
  PrimColumn a =>
  Builder Encoder Decoder a
prim =
  column def primValue

uid ::
  ∀ i a as .
  ProdWith a as =>
  Builder Encoder Decoder i ->
  NP (Builder Encoder Decoder) as ->
  Builder Encoder Decoder (Uid i a)
uid i a =
  prod (i :* (flatten a) :* Nil)
