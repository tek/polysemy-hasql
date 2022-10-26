module Polysemy.Hasql.Table.Dsl.Sum where

import Data.Functor.Contravariant.Divisible (choose)
import Exon (exon)
import Generics.SOP (
  All,
  All2,
  HIndex (hindex),
  I,
  K (K),
  NP (Nil, (:*)),
  NS (S, Z),
  SListI,
  SListI2,
  SOP (SOP),
  Top,
  hcfoldMap,
  hcmap,
  hcollapse,
  hctraverse_,
  hczipWith3,
  hmap,
  hsequence,
  htraverse_,
  hzipWith,
  unPOP,
  unSOP,
  )
import Generics.SOP.GGP (gfrom, gto)
import qualified Hasql.Decoders as Decoders
import Hasql.Decoders (Row)
import qualified Hasql.Encoders as Encoders
import Hasql.Encoders (Params)
import Lens.Micro.Extras (view)
import Polysemy.Db.SOP.Constraint (ConstructSOP, DataName (dataNameString), ReifySOP)
import Polysemy.Db.SOP.Contravariant (sequenceContravariantNP)
import Polysemy.Db.Text.DbIdentifier (dbIdentifier, dbIdentifierT)

import Polysemy.Hasql.QueryParams (unconsNS)
import qualified Polysemy.Hasql.Table.Dsl.Data.Builder as Builder
import Polysemy.Hasql.Table.Dsl.Data.Builder (Builder (Builder), Decoder (Decoder), Encoder (Encoder))
import Polysemy.Hasql.Table.Dsl.Data.PgColumn (PgColumn (PgColumn), PgField (PgFieldColumn), PgProdName (PgProdName), PgType (PgTypeProd))
import Polysemy.Hasql.Table.Dsl.Names (DataConNames (dataConNames), DataFieldNames (dataFieldNames), dataFieldNames)
import Polysemy.Hasql.Table.Dsl.Product (fieldColumn)
import Polysemy.Hasql.Table.ReadNull (ignoreDecoder)

readNull ::
  ∀ as .
  SListI as =>
  NP Decoder as ->
  Row ()
readNull rs =
  htraverse_ (void . const ignoreDecoder) rs

readNulls ::
  ∀ ass .
  SListI2 ass =>
  NP (NP Decoder) ass ->
  Row ()
readNulls cons =
  hctraverse_ (Proxy @SListI) readNull cons

sumRows ::
  All2 Top ass =>
  NP (NP Decoder) ass ->
  Int ->
  Row (NS (NP I) ass)
sumRows ((con :: NP Decoder as) :* cons) 0 =
  Z <$> (hsequence (hmap (view #decodeValue) con)) <* readNulls cons
sumRows (con :* cons) index = do
  readNull con
  S <$> sumRows cons (index - 1)
sumRows Nil index =
  fail [exon|invalid index into sum type in database: #{show index}|]

indexRow :: Row Int
indexRow =
  fromIntegral <$> Decoders.column (Decoders.nonNullable Decoders.int8)

ignoreEncoder :: Params a
ignoreEncoder =
  const Nothing >$< Encoders.param (Encoders.nullable Encoders.int8)

writeNull ::
  ∀ a as .
  SListI as =>
  NP Encoder as ->
  Params a
writeNull rs =
  hcfoldMap (Proxy @Top) (contramap (const ()) . view #encodeNulls) rs

writeNulls ::
  ∀ a ass .
  SListI2 ass =>
  NP (NP Encoder) ass ->
  Params a
writeNulls cons =
  hcfoldMap (Proxy @SListI) writeNull cons

sumParams ::
  All2 Top ass =>
  NP (NP Encoder) ass ->
  Params (NS (NP I) ass)
sumParams (con :* cons) =
  choose unconsNS inhabited uninhabited
  where
    inhabited =
      sequenceContravariantNP con ^. #encodeValue <> writeNulls cons
    uninhabited =
      writeNull con <> sumParams cons
sumParams Nil =
  mempty

indexParams :: Params Int
indexParams =
  fromIntegral >$< Encoders.param (Encoders.nonNullable Encoders.int8)

indexColumn :: Text -> (Text, PgField ())
indexColumn typeName =
  ([exon|ph_sum_index__#{typeName}|], "bigint")

conField :: ∀ x . All Top x => NP (K Text) x -> K Text x -> NP (Builder Encoder Decoder) x -> K (Text, PgField ()) x
conField fs (K conName) cons =
  K (name, PgFieldColumn col)
  where
    col = PgColumn (PgTypeProd (PgProdName name) (hcollapse (hzipWith fieldColumn fs cons))) mempty
    name =
      dbIdentifierT conName

sum ::
  ∀ a ass name .
  ReifySOP a ass =>
  DataName a name =>
  ConstructSOP a ass =>
  DataConNames a ass =>
  DataFieldNames a ass =>
  NP (NP (Builder Encoder Decoder)) ass ->
  Builder Encoder Decoder a
sum conBuilders =
  Builder {
    column = PgFieldColumn (PgColumn (PgTypeProd (PgProdName typeName) (indexColumn typeName : conTypes)) mempty),
    decoder = Decoder decodeValue unit,
    encoder = Encoder encodeValue mempty
  }
  where
    conTypes =
      hcollapse (hczipWith3 (Proxy @(All Top)) conField (unPOP (dataFieldNames @a)) (dataConNames @a) conBuilders)
    decodeValue =
      gto . SOP <$> (sumRows (hcmap (Proxy @SListI) (hmap (view #decoder)) conBuilders) =<< indexRow)
    encodeValue =
      unSOP . gfrom >$< (indexEncoder <> sumParams (hcmap (Proxy @SListI) (hmap (view #encoder)) conBuilders))
    indexEncoder =
      hindex >$< indexParams
    typeName =
      dbIdentifier (dataNameString @a)
