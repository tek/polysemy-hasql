module Sqel.Uid where

import Sqel.Data.Dd (Dd, DdK, DdType, DdTypeName, type (:>) ((:>)))
import Sqel.Data.Uid (Uid)
import Sqel.Merge (merge)
import Sqel.Names.Rename (Rename, rename)
import Sqel.Names.Set (SetTypeName)
import Sqel.Product (Product (prod))
import qualified Sqel.Type as T
import Sqel.Type (type (*>), type (>))

type UidDd si sa =
  T.TypeName (DdTypeName sa) (T.Prod (Uid (DdType si) (DdType sa))) *> T.Name "id" si > T.Merge sa

type UidColumn :: Type -> Type -> DdK -> DdK -> DdK -> Constraint
class (
    DdType si ~ i,
    DdType sa ~ a
  ) => UidColumn i a si sa s | i a si sa -> s where
    uidColumn :: Dd si -> Dd sa -> Dd s

instance (
    DdType si ~ i,
    DdType sa ~ a,
    Product (Uid i a) (Dd si :> Dd (T.Merge sa)) s
  ) => UidColumn i a si sa s where
    uidColumn i a =
      prod (i :> merge a)

uid ::
  ∀ (i :: Type) (a :: Type) (si :: DdK) (sa :: DdK) (s :: DdK) .
  UidColumn i a si sa s =>
  Dd si ->
  Dd sa ->
  Dd s
uid =
  uidColumn

uidAs ::
  ∀ (name :: Symbol) (i :: Type) (a :: Type) (si :: DdK) (sa :: DdK) (s :: DdK) .
  UidColumn i a si sa s =>
  Rename s (SetTypeName s name) =>
  Dd si ->
  Dd sa ->
  Dd (SetTypeName s name)
uidAs i a =
  rename (uidColumn i a)
