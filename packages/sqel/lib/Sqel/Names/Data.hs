module Sqel.Names.Data where

import Generics.SOP.GGP (GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (
  ConstructorInfo (Constructor, Infix, Record),
  DatatypeInfo (ADT, Newtype),
  FieldInfo (FieldInfo),
  )
import Prelude hiding (type (@@))
import Type.Errors.Pretty (type (<>))


type family ConNs (fs :: [FieldInfo]) :: [Symbol] where
  ConNs '[] = '[]
  ConNs ('FieldInfo n : fs) = n : ConNs fs

type family AdtNs (cons :: [ConstructorInfo]) :: [(Symbol, [Symbol])] where
  AdtNs '[] = '[]
  AdtNs ('Record conName fs : cons) =
    '(conName, ConNs fs) : AdtNs cons
  AdtNs ('Constructor conName : _) =
    TypeError ("Not a record: " <> conName)
  AdtNs ('Infix conName _ _ : _) =
    TypeError ("Not a record: " <> conName)

type family Ns (info :: DatatypeInfo) :: [(Symbol, [Symbol])] where
  Ns ('ADT _ _ cons _) =
    AdtNs cons
  Ns ('Newtype _ name _) =
    TypeError ("Newtype used for composite column: " <> name)

type family SumConNames (a :: Type) :: [(Symbol, [Symbol])] where
  SumConNames a = Ns (GDatatypeInfoOf a)

type ProdNames' :: Type -> [(Symbol, [Symbol])] -> [Symbol]
type family ProdNames' (a :: Type) (names :: [(Symbol, [Symbol])]) :: [Symbol] where
  ProdNames' _ '[ '(_, names)] = names
  ProdNames' a '[] = TypeError ("Tried using empty type as a product: " <> a)
  ProdNames' a _ = TypeError ("Tried using sum type as a product: " <> a)

type family ProdNames (a :: Type) :: [Symbol] where
  ProdNames a = ProdNames' a (Ns (GDatatypeInfoOf a))
