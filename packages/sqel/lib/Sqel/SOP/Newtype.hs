module Sqel.SOP.Newtype where

import Generics.SOP.GGP (GCode, GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (DatatypeInfo (ADT, Newtype))
import Type.Errors (ErrorMessage)
import Type.Errors.Pretty (type (%), type (<>))
import Unsafe.Coerce (unsafeCoerce)

import Sqel.SOP.Error (Quoted, QuotedType)
import Sqel.SOP.Fundeps (DummyDep)

type NoGenericError a =
  "The type " <> QuotedType a <> " does not have an instance of " <> Quoted "Generic" <> "." %
  "You can add it like this:" %
  Quoted "newtype MyType = MyType Text deriving Generic" %
  "If you want to use " <> Quoted "Coercible" <> " instead, use " <> Quoted "primCoerce" <> "."

type NotNewtypeError a =
  "The type " <> QuotedType a <> " is not a newtype."

type UN_CheckNewtype :: ErrorMessage -> [[Type]] -> DatatypeInfo -> Type -> Type -> Constraint
class UN_CheckNewtype err ass info a w | ass info a -> w where
  un_checkNewtype_unwrap :: a -> w
  un_checkNewtype_wrap :: w -> a

instance UN_CheckNewtype err '[ '[w] ] ('Newtype mn dn ci) a w where
  un_checkNewtype_unwrap = unsafeCoerce
  un_checkNewtype_wrap = unsafeCoerce

instance (
  TypeError (err % NotNewtypeError a),
  '[ '[w] ] ~ ass
  ) => UN_CheckNewtype err ass ('ADT mn dn ci si) a w where
  un_checkNewtype_unwrap = error "not a newtype"
  un_checkNewtype_wrap = error "not a newtype"

type UN_CheckGeneric :: ErrorMessage -> [[Type]] -> DatatypeInfo -> Type -> Type -> Constraint
class UN_CheckGeneric err ass info a w | ass info a -> w where
  un_checkGeneric_unwrap :: a -> w
  un_checkGeneric_wrap :: w -> a

instance (
    UN_CheckNewtype err '[] info a w
  ) => UN_CheckGeneric err '[] info a w where
    un_checkGeneric_unwrap = un_checkNewtype_unwrap @err @'[] @info
    un_checkGeneric_wrap = un_checkGeneric_wrap @err @'[] @info

instance (
    UN_CheckNewtype err (x : xs) info a w
  ) => UN_CheckGeneric err (x : xs) info a w where
    un_checkGeneric_unwrap = un_checkNewtype_unwrap @err @(x : xs) @info
    un_checkGeneric_wrap = un_checkGeneric_wrap @err @(x : xs) @info

instance {-# incoherent #-} (
    TypeError (err % NoGenericError a),
    DummyDep ass w
  ) => UN_CheckGeneric err ass info a w where
    un_checkGeneric_unwrap = error "no Generic"
    un_checkGeneric_wrap = error "no Generic"

type UnwrapNewtype :: ErrorMessage -> Type -> Type -> Constraint
class UnwrapNewtype err a w | a -> w where
  unwrapNewtype :: a -> w
  wrapNewtype :: w -> a

instance (
  UN_CheckGeneric err (GCode a) (GDatatypeInfoOf a) a w
  ) => UnwrapNewtype err a w where
    unwrapNewtype = un_checkGeneric_unwrap @err @(GCode a) @(GDatatypeInfoOf a)
    wrapNewtype = un_checkGeneric_wrap @err @(GCode a) @(GDatatypeInfoOf a)
