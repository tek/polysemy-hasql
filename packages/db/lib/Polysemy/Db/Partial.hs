module Polysemy.Db.Partial where

import Generics.SOP (All, I(I), NP ((:*)), NS(Z), SOP(SOP), Top, hpure, hzipWith, unZ)
import Generics.SOP.GGP (gfrom, gto)

import qualified Polysemy.Db.Data.PartialField as PartialField
import Polysemy.Db.Data.PartialField (FieldUpdate(FieldUpdate), PartialField)
import Polysemy.Db.Data.PartialFields (PartialFields(PartialFields), PartialFieldsT, Types)
import Polysemy.Db.SOP.Constraint (ConstructSOP, ReifySOP, symbolText)

partial ::
  ∀ d .
  All Top (Types d) =>
  PartialFieldsT d
partial =
  PartialFields (hpure PartialField.Keep :: NP PartialField (Types d))

field ::
  ∀ (name :: Symbol) (a :: *) .
  a ->
  FieldUpdate name a
field =
  FieldUpdate

class InsertField (ds :: [*]) (fs :: [Symbol]) name a | ds -> a where
  insertField :: PartialFields d ds fs -> FieldUpdate name a -> PartialFields d ds fs

instance (
    KnownSymbol name
  ) => InsertField (a : ds) (name : fs) name a where
  insertField (PartialFields (_ :* fs)) (FieldUpdate a) =
    PartialFields (PartialField.Update (symbolText @name) a :* fs)

instance {-# overlappable #-} (
    InsertField ds fs name a
  ) => InsertField (d : ds) (_name : fs) name a where
  insertField (PartialFields (f :* fs)) a =
    PartialFields (f :* fs')
    where
      PartialFields fs' =
        insertField @ds @fs (PartialFields fs) a

(.>) ::
  ∀ d ds fs name a .
  InsertField ds fs name a =>
  PartialFields d ds fs ->
  FieldUpdate name a ->
  PartialFields d ds fs
(.>) =
  insertField @ds @fs @name @a

infixl 5 .>

class UpdatePartial (ds :: [*]) where
  updatePartialNP :: PartialFields a ds fs -> NP I ds -> NP I ds

instance (
    All Top ds
  ) => UpdatePartial ds where
  updatePartialNP (PartialFields update) record =
    hzipWith extract record update
    where
      extract old = \case
        PartialField.Update _ a -> I a
        PartialField.Keep -> old

updatePartial ::
  ReifySOP d '[ds] =>
  ConstructSOP d '[ds] =>
  UpdatePartial ds =>
  PartialFields d ds fs ->
  d ->
  d
updatePartial update (gfrom -> SOP (unZ -> d)) =
  gto (SOP (Z (updatePartialNP update d)))
