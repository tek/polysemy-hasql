module Sqel.Names.Error where

import Sqel.SOP.Error (QuotedError)
import Prelude hiding (type (@@))
import Type.Errors (ErrorMessage)

type CountMismatch :: Symbol -> ErrorMessage -> Nat -> Nat -> ErrorMessage
type family CountMismatch desc a spec actual where
  CountMismatch desc a spec actual =
    "The " <> desc <> " " <> QuotedError a <> " has " <> actual <> " fields, but the expression specifies " <>
    spec <> "."

type family NoPrimType (a :: Type) :: k where
  NoPrimType a =
    TypeError ("Can't set type name on primitive column of type " <> a)
