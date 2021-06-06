module Polysemy.Db.Data.FieldId where

import Fcf (Eval, Exp, type (@@), Pure1)
import Fcf.Class.Functor (FMap)
import Type.Errors (ErrorMessage(ShowType, Text))
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
    [text|#{symbolText @name}_#{natVal (Proxy @index)}|]
  fieldIdText =
    dbIdentifier [text|#{symbolString @name}_#{natVal (Proxy @index)}|]

data FieldIdSymbol :: FieldId -> Exp ErrorMessage

type instance Eval (FieldIdSymbol ('NamedField name)) = 'Text name
type instance Eval (FieldIdSymbol ('NumberedField name index)) = name <> "_" <> 'ShowType index

type family JoinCommaFieldIds (ids :: [FieldId]) :: ErrorMessage where
  JoinCommaFieldIds ids =
    JoinComma (FMap FieldIdSymbol @@ ids)

type NamedFields names =
  FMap (Pure1 'NamedField) @@ names
