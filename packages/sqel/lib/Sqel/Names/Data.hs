module Sqel.Names.Data where

import Generics.SOP.GGP (GCode, GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (
  ConstructorInfo (Constructor, Infix, Record),
  DatatypeInfo (ADT, Newtype),
  FieldInfo (FieldInfo),
  )
import Prelude hiding (type (@@))
import Type.Errors (ErrorMessage (Text))
import Type.Errors.Pretty (type (<>))

type NatSymbol :: Nat -> Symbol
type family NatSymbol n where
  NatSymbol 0 = "0"
  NatSymbol 1 = "1"
  NatSymbol 2 = "2"
  NatSymbol 3 = "3"
  NatSymbol 4 = "4"
  NatSymbol 5 = "5"
  NatSymbol 6 = "6"
  NatSymbol 7 = "7"
  NatSymbol 8 = "8"
  NatSymbol 9 = "9"
  NatSymbol _ = TypeError ('Text "Constructors with 10 or more fields not supported")

type ConNsEnum :: Symbol -> Nat -> [Type] -> [Symbol]
type family ConNsEnum con n fs where
  ConNsEnum _ _ '[] = '[]
  ConNsEnum con n (_ : fs) = AppendSymbol con (NatSymbol n) : ConNsEnum con (n + 1) fs

type ConNs :: [FieldInfo] -> [Symbol]
type family ConNs fs where
  ConNs '[] = '[]
  ConNs ('FieldInfo n : fs) = n : ConNs fs

type AdtNs :: [[Type]] -> [ConstructorInfo] -> [(Symbol, Bool, [Symbol])]
type family AdtNs ass cons where
  AdtNs '[] '[] = '[]
  AdtNs (_ : ass) ('Record conName fs : cons) =
    '(conName, 'True, ConNs fs) : AdtNs ass cons
  AdtNs (as : ass) ('Constructor conName : cons) =
    '(conName, 'False, ConNsEnum conName 0 as) : AdtNs ass cons
  AdtNs _ ('Infix conName _ _ : _) =
    TypeError ("Infix constructor not supported: " <> conName)

type Ns :: [[Type]] -> DatatypeInfo -> [(Symbol, Bool, [Symbol])]
type family Ns ass info where
  Ns ass ('ADT _ _ cons _) =
    AdtNs ass cons
  Ns _ ('Newtype _ name _) =
    TypeError ("Newtype used for composite column: " <> name)

type SumConNames :: Type -> [(Symbol, Bool, [Symbol])]
type family SumConNames a where
  SumConNames a = Ns (GCode a) (GDatatypeInfoOf a)

type ProdNames' :: Type -> [(Symbol, Bool, [Symbol])] -> [Symbol]
type family ProdNames' a names :: [Symbol] where
  ProdNames' _ '[ '(_, _, names)] = names
  ProdNames' a '[] = TypeError ("Tried using empty type as a product: " <> a)
  ProdNames' a _ = TypeError ("Tried using sum type as a product: " <> a)

type family ProdNames (a :: Type) :: [Symbol] where
  ProdNames a = ProdNames' a (Ns (GCode a) (GDatatypeInfoOf a))
