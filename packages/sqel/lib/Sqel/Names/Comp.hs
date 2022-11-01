module Sqel.Names.Comp where

import Generics.SOP (NP)
import Generics.SOP.GGP (GCode)
import Prelude hiding (type (@@))
import Type.Errors (ErrorMessage (Text))
import Type.Errors.Pretty (type (%))

import Sqel.Data.Dd (Comp (Prod), CompInc (Merge, Nest), DdK (DdK), ProdType (Con), Struct (Comp, Prim))
import Sqel.Names.Amend (AmendName, AmendNamesFor, AmendTypeName)
import Sqel.Names.Data (ProdNames, SumConNames)
import Sqel.Names.Rename (RenameN, RenameN2)

type ProdNamed :: Type -> [DdK] -> [DdK] -> Constraint
type ProdNamed a s0 s1 =
  (
    s1 ~ AmendNamesFor "product type" ('ShowType a) s0 (ProdNames a),
    RenameN NP s0 s1
  )

data Con (name :: Symbol) (names :: [Symbol])

type family Cons (ass :: [[Type]]) (names :: [(Symbol, [Symbol])]) :: [Type] where
  Cons '[] '[] = '[]
  Cons (_ : ass) ('(cn, ns) : names) = Con cn ns : Cons ass names
  Cons ass names = TypeError ("Cons:" % ass % names)

type AmendConName :: DdK -> Type -> DdK
type family AmendConName s a where
  AmendConName ('DdK sel p t ('Comp tsel ('Prod ('Con as)) 'Nest s)) (Con cn ns) =
    AmendName (AmendTypeName ('DdK sel p t ('Comp tsel ('Prod ('Con as)) 'Nest (AmendNamesFor "constructor" ('Text cn) s ns))) cn) cn
  AmendConName ('DdK sel p t ('Comp tsel ('Prod ('Con as)) 'Merge s)) (Con cn _) =
    AmendName (AmendTypeName ('DdK sel p t ('Comp tsel ('Prod ('Con as)) 'Merge (AmendNamesFor "constructor" ('Text cn) s '[cn]))) cn) cn
  AmendConName ('DdK sel p t 'Prim) (Con cn _) =
    AmendName ('DdK sel p t 'Prim) cn

type AmendConsNames :: [DdK] -> [Type] -> [DdK]
type family AmendConsNames s as where
  AmendConsNames '[] '[] = '[]
  AmendConsNames (s : ss) (a : as) = AmendConName s a : AmendConsNames ss as
  AmendConsNames s as = TypeError ("AmendConsNames:" % s % as)

type SumNamed :: Type -> [DdK] -> [DdK] -> Constraint
type SumNamed a s0 s1 =
  (
    s1 ~ AmendConsNames s0 (Cons (GCode a) (SumConNames a)),
    RenameN2 NP s0 s1
  )
