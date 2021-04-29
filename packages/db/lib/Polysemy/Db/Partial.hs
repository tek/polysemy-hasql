module Polysemy.Db.Partial where

import Generics.SOP (All, I(I), NP ((:*)), NS(Z), SOP(SOP), Top, hpure, hzipWith, unZ)
import Generics.SOP.GGP (gfrom, gto)

import qualified Polysemy.Db.Data.PartialField as PartialField
import Polysemy.Db.Data.PartialField (FieldUpdate(FieldUpdate), PartialField)
import Polysemy.Db.Data.PartialFields (FieldTypes, PartialFields(PartialFields))
import Polysemy.Db.SOP.Constraint (ConstructSOP, ReifySOP, symbolText)
import Polysemy.Db.SOP.FieldNames (FieldNames)

partial ::
  ∀ d .
  All Top (FieldTypes d) =>
  PartialFields d
partial =
  PartialFields (hpure PartialField.Keep :: NP PartialField (FieldTypes d))

field ::
  ∀ (name :: Symbol) (a :: *) .
  a ->
  FieldUpdate name a
field =
  FieldUpdate

class InsertField (ds :: [*]) (fs :: [Symbol]) (name :: Symbol) (a :: *) | ds -> a where
  insertField :: NP PartialField ds -> FieldUpdate name a -> NP PartialField ds

instance (
    KnownSymbol name
  ) => InsertField (a : ds) (name : fs) name a where
  insertField (_ :* fs) (FieldUpdate a) =
    PartialField.Update (symbolText @name) a :* fs

instance {-# overlappable #-} (
    InsertField ds fs name a
  ) => InsertField (d : ds) (_name : fs) name a where
  insertField (f :* fs) a =
    f :* insertField @ds @fs fs a

class InsertField' (d :: *) (name :: Symbol) (a :: *) | d -> a where
  insertField' :: NP PartialField (FieldTypes d) -> FieldUpdate name a -> NP PartialField (FieldTypes d)

(.>) ::
  ∀ d name a .
  InsertField (FieldTypes d) (FieldNames d) name a =>
  PartialFields d ->
  FieldUpdate name a ->
  PartialFields d
(.>) (PartialFields fs) f =
  PartialFields (insertField @(FieldTypes d) @(FieldNames d) fs f)

infixl .>

class UpdatePartial (d :: *) where
  updatePartialNP :: PartialFields d -> NP I (FieldTypes d) -> NP I (FieldTypes d)

instance (
    All Top (FieldTypes d)
  ) => UpdatePartial d where
  updatePartialNP (PartialFields update) record =
    hzipWith extract record update
    where
      extract old = \case
        PartialField.Update _ a -> I a
        PartialField.Keep -> old

updatePartial ::
  ReifySOP d '[FieldTypes d] =>
  ConstructSOP d '[FieldTypes d] =>
  UpdatePartial d =>
  PartialFields d ->
  d ->
  d
updatePartial update (gfrom -> SOP (unZ -> d)) =
  gto (SOP (Z (updatePartialNP update d)))
