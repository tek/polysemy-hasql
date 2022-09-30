module Polysemy.Db.Data.FieldId where

import Exon (exon)
import Fcf (Eval, Exp, Pure1, type (@@))
import Fcf.Class.Functor (FMap)
import Prelude hiding (type (@@))
import Type.Errors (ErrorMessage (Text))
import Type.Errors.Pretty (type (<>))

import Polysemy.Db.SOP.Constraint (symbolString, symbolText)
import Polysemy.Db.SOP.Error (JoinComma)
import Polysemy.Db.Text.DbIdentifier (dbIdentifier)
import Polysemy.Db.Text.Quote (dquote)

data FieldId =
  NamedField Symbol
  |
  NumberedField Symbol Nat

class FieldIdText (id :: FieldId) where
  fieldIdTextRaw :: Text
  fieldIdText :: Text

instance KnownSymbol name => FieldIdText ('NamedField name) where
  fieldIdTextRaw =
    symbolText @name
  fieldIdText =
    dbIdentifier (symbolString @name)

quotedFieldId ::
  ∀ id .
  FieldIdText id =>
  Text
quotedFieldId =
  dquote (fieldIdText @id)

instance (
    KnownSymbol name,
    KnownNat index
  ) => FieldIdText ('NumberedField name index) where
  fieldIdTextRaw =
    [exon|#{symbolText @name}_#{show (natVal (Proxy @index))}|]
  fieldIdText =
    dbIdentifier [exon|#{symbolString @name}_#{show (natVal (Proxy @index))}|]

data FieldIdSymbol :: FieldId -> Exp ErrorMessage

type instance Eval (FieldIdSymbol ('NamedField name)) = 'Text name
type instance Eval (FieldIdSymbol ('NumberedField name index)) = name <> "_" <> 'ShowType index

type family JoinCommaFieldIds (ids :: [FieldId]) :: ErrorMessage where
  JoinCommaFieldIds ids =
    JoinComma (FMap FieldIdSymbol @@ ids)

type NamedFields names =
  FMap (Pure1 'NamedField) @@ names
