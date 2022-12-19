module Sqel.Names.Comp where

import Generics.SOP (NP)
import Generics.SOP.GGP (GCode)
import Prelude hiding (type (@@))
import Type.Errors (ErrorMessage (Text))
import Type.Errors.Pretty (type (%))

import Sqel.Data.Dd (Comp (Prod), CompInc (Merge), DdK (DdK), ProdType (Con), Struct (Comp, Prim))
import Sqel.Names.Amend (AmendName, AmendNamesFor, AmendTypeName)
import Sqel.Names.Data (ProdNames, SumConNames)
import Sqel.Names.Rename (RenameN, RenameN2)
import Sqel.Names.Set (SetName)

type ProdNamed :: Type -> [DdK] -> [DdK] -> Constraint
type ProdNamed a s0 s1 =
  (
    s1 ~ AmendNamesFor "product type" ('ShowType a) s0 (ProdNames a),
    RenameN NP s0 s1
  )

data Con (name :: Symbol) (record :: Bool) (names :: [Symbol])

type family Cons (ass :: [[Type]]) (names :: [(Symbol, Bool, [Symbol])]) :: [Type] where
  Cons '[] '[] = '[]
  Cons (_ : ass) ('(cn, record, ns) : names) = Con cn record ns : Cons ass names
  Cons ass names = TypeError ("Cons:" % ass % names)

type family Con1Name (record :: Bool) (conName :: Symbol) (fieldName :: Symbol) :: Symbol where
  Con1Name 'True _ fieldName = fieldName
  Con1Name 'False conName _ = conName

type AmendConName :: DdK -> Type -> DdK
type family AmendConName s a where
  AmendConName ('DdK sel p t ('Comp tsel ('Prod ('Con '[a])) 'Merge s)) (Con cn record '[ns]) =
    AmendName (AmendTypeName ('DdK sel p t ('Comp tsel ('Prod ('Con '[a])) 'Merge (AmendNamesFor "constructor" ('Text cn) s '[Con1Name record cn ns]))) cn) cn
  AmendConName ('DdK sel p t ('Comp tsel ('Prod ('Con as)) i s)) (Con cn _ ns) =
    AmendName (AmendTypeName ('DdK sel p t ('Comp tsel ('Prod ('Con as)) i (AmendNamesFor "constructor" ('Text cn) s ns))) cn) cn
  AmendConName ('DdK sel p t 'Prim) (Con cn _ _) =
    AmendName ('DdK sel p t 'Prim) cn

type AmendConsNames :: [DdK] -> [Type] -> [DdK]
type family AmendConsNames s as where
  AmendConsNames '[] '[] = '[]
  AmendConsNames (s : ss) (a : as) = AmendConName s a : AmendConsNames ss as
  AmendConsNames s as = TypeError ("AmendConsNames:" % s % as)

type family SetCon1Name (s :: DdK) (name :: Symbol) :: DdK where
  SetCon1Name ('DdK sel p t ('Comp tsel ('Prod ('Con as)) 'Merge '[s])) name =
    'DdK sel p t ('Comp tsel ('Prod ('Con as)) 'Merge '[SetName s name])

type SumNamed :: Type -> [DdK] -> [DdK] -> Constraint
type SumNamed a s0 s1 =
  (
    s1 ~ AmendConsNames s0 (Cons (GCode a) (SumConNames a)),
    RenameN2 NP s0 s1
  )
