module Polysemy.Hasql.Table.Dsl.Names where

import Fcf (Fst, Snd, type (@@))
import Fcf.Class.Functor (FMap)
import Generics.SOP (All, All2, AllZip, AllZip2, K (K), NP, POP, hcpure, htrans, unK)
import Generics.SOP.GGP (GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (
  ConstructorInfo (Constructor, Infix, Record),
  DatatypeInfo (ADT, Newtype),
  FieldInfo (FieldInfo),
  )
import Polysemy.Db.SOP.Constraint (Coded, Top2, symbolText)
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

class SymbolK (n :: Symbol) where
  symbolK :: K Text n

instance KnownSymbol n => SymbolK n where
  symbolK = K (symbolText @n)

class DataFieldNames (a :: Type) (ass :: [[Type]]) | a -> ass where
  dataFieldNames :: POP (K Text) ass

instance (
    Coded a ass,
    All2 SymbolK fs,
    fs ~ FMap Snd @@ Ns (GDatatypeInfoOf a),
    AllZip2 Top2 fs ass
  ) => DataFieldNames a ass where
    dataFieldNames =
      htrans (Proxy @Top2) (K . unK) (hcpure (Proxy @SymbolK) symbolK :: POP (K Text) fs)

class DataConNames (a :: Type) (ass :: [[Type]])  where
  dataConNames :: NP (K Text) ass

instance (
    Coded a ass,
    All SymbolK cons,
    cons ~ FMap Fst @@ Ns (GDatatypeInfoOf a),
    AllZip Top2 cons ass
  ) => DataConNames a ass where
  dataConNames =
      htrans (Proxy @Top2) (K . unK) (hcpure (Proxy @SymbolK) symbolK :: NP (K Text) cons)
