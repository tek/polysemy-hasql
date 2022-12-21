module Sqel.ProductArg where

import Generics.SOP (NP (Nil, (:*)))
import Type.Errors (DelayError, ErrorMessage)
import Type.Errors.Pretty (type (%), type (<>))

import Sqel.Data.Dd (Dd, DdK, (:>) ((:>)))
import Sqel.Prim (Prims, unPrims)
import Sqel.SOP.Error (Quoted, QuotedType)

type InvalidElem name i arg =
  DelayError (
    "Element number " <> i <> " in the call to " <> Quoted name <> " has type " <> QuotedType arg <> "." %
    "Columns should only be constructed with combinators like " <> Quoted "prim" <> ", " <> Quoted "prod" <> "," %
    Quoted "column" <> " that return the proper type, " <> Quoted "Dd" <> "." %
    "Consult the module " <> Quoted "Sqel.Combinators" <> " for the full API."
  )

type InvalidArg name arg =
  DelayError (
    "The argument to " <> Quoted name <> " has type " <> QuotedType arg <> "." %
    "Composite type combinators should be applied to a sequence of columns combined by " <> Quoted ":>" <> ":" %
    ">>> " <> name <> " (prim :> columnAs @\"name\" prim :> prim)" %
    "or, for a type consisting solely of primitive fields:" %
    ">>> " <> name <> " prims" %
    "Consult the module " <> Quoted "Sqel.Combinators" <> " for the full API."
  )

class Dummy arg s | arg -> s where

-- TODO use this instead
class FundepError (err :: Void) arg s | arg -> s where

type ProductElem :: Symbol -> Nat -> Type -> DdK -> Constraint
class ProductElem name i arg s | arg -> s where
  productElem :: arg -> Dd s

instance (
    s0 ~ s
  ) => ProductElem name i (Dd s0) s where
    productElem = id

instance {-# overlappable #-} (
    InvalidElem name i arg,
    Dummy arg s
  ) => ProductElem name i arg s where
    productElem = error "unreachable"

-- | The parameter @a@ here is necessary to pass the composite type through to @prims@.
type ProductElems :: ErrorMessage -> Symbol -> Nat -> Type -> Type -> [DdK] -> Constraint
class ProductElems err name i a arg s | a arg -> s where
  productElems :: Proxy err -> arg -> NP Dd s

instance ProductElems err name i a (NP Dd s) s where
  productElems _ = id

instance a ~ b => ProductElems err name i a (Prims b s) s where
  productElems _ = unPrims

instance (
    ProductElem name i l s0,
    ProductElems err name (i + 1) a r s
  ) => ProductElems err name i a (l :> r) (s0 : s) where
    productElems err (l :> r) =
      productElem @name @i l :* productElems @err @name @(i + 1) @a err r

instance ProductElems err name i a (Dd s) '[s] where
  productElems _ d =
      d :* Nil

instance (
    TypeError ("The argument to " <> Quoted name <> " is of function type."),
    Dummy (x -> y) s
  ) => ProductElems err name i a (x -> y) s where
  productElems _ _ =
    error "unreachable"

instance {-# overlappable #-} (
    InvalidArg name arg,
    Dummy arg s
  ) => ProductElems err name i a arg s where
    productElems _ _ = error "unreachable"

type ProductArg :: Type -> Symbol -> Type -> [DdK] -> Constraint
class ProductArg a name arg s | a arg -> s where
  productArg :: arg -> NP Dd s

instance (
    error ~ InvalidArg name arg,
    ProductElems error name 1 a arg s
  ) => ProductArg a name arg s where
    productArg = productElems @_ @name @1 @a (Proxy @error)
