module Polysemy.Hasql.Table.Dsl.Product where

import Generics.SOP (
  All,
  I,
  K (K),
  NP,
  NS (Z),
  Projection,
  SOP (SOP),
  Top,
  hcollapse,
  hd,
  hmap,
  hsequence,
  hzipWith,
  projections,
  unI,
  unPOP,
  unSOP,
  unZ,
  type  (-.->) (Fn),
  )
import Generics.SOP.GGP (gfrom, gto)
import Lens.Micro.Extras (view)
import Polysemy.Db.SOP.Constraint (ConstructProd, DataName (dataNameString), ReifyProd)
import Polysemy.Db.Text.DbIdentifier (dbIdentifier)

import qualified Polysemy.Hasql.Table.Dsl.Data.Builder as Builder
import Polysemy.Hasql.Table.Dsl.Data.Builder (Builder (Builder), Decoder, Encoder)
import Polysemy.Hasql.Table.Dsl.Data.PgColumn (
  PgColumn (PgColumn),
  PgField (PgFieldColumn, PgFieldFlatten),
  PgProdName (PgProdName),
  PgType (PgTypeProd),
  )
import Polysemy.Hasql.Table.Dsl.Names (DataFieldNames (dataFieldNames))

prodParams ::
  ∀ as .
  All Top as =>
  NP Encoder as ->
  Encoder (NP I as)
prodParams np =
  mconcat (hcollapse qps)
  where
    qps :: NP (K (Encoder (NP I as))) as
    qps =
      hzipWith qp np (projections :: NP (Projection I as) as)
    {-# inline qps #-}
    qp :: ∀ a . Encoder a -> Projection I as a -> K (Encoder (NP I as)) a
    qp par (Fn proj) =
      K (contramap (unI . proj . K) par)
    {-# inline qp #-}
{-# inline prodParams #-}

fieldColumn ::
  ∀ x .
  K Text x ->
  Builder Encoder Decoder x ->
  K (Text, PgField ()) x
fieldColumn (K n) c =
  K (n, coerce (c ^. #column) :: PgField ())

class ProdWith a (as :: [Type]) | a -> as where
  prodWith :: Bool -> NP (Builder Encoder Decoder) as -> Builder Encoder Decoder a

instance (
    ReifyProd a as,
    DataName a name,
    ConstructProd a as,
    DataFieldNames a '[as]
  ) => ProdWith a as where
    prodWith flat np =
      Builder {
        column,
        decoder = gto . SOP . Z <$> hsequence (hmap (view #decoder) np),
        encoder = unZ . unSOP . gfrom >$< prodParams (hmap (view #encoder) np)
      }
      where
        column
          | flat = PgFieldFlatten prodName fields
          | otherwise = PgFieldColumn (PgColumn (PgTypeProd prodName fields) mempty)
        fields =
          hcollapse (hzipWith fieldColumn names np)
        names =
          hd (unPOP (dataFieldNames @a))
        prodName =
          PgProdName (dbIdentifier (dataNameString @a))

prod ::
  ∀ a as .
  ProdWith a as =>
  NP (Builder Encoder Decoder) as ->
  Builder Encoder Decoder a
prod =
  prodWith False

flatten ::
  ∀ a as .
  ProdWith a as =>
  NP (Builder Encoder Decoder) as ->
  Builder Encoder Decoder a
flatten =
  prodWith True
