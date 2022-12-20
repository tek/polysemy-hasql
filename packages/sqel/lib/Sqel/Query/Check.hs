module Sqel.Query.Check where

import Fcf (Eval, Exp, Find, Not, type (<=<), type (@@))
import Fcf.Class.Foldable (All)
import Fcf.Class.Functor (FMap)
import Prelude hiding (type (@@))
import Type.Errors (ErrorMessage)
import Type.Errors.Pretty (type (%), type (<>))

import Sqel.Data.Dd (Comp (Sum), CompInc (Merge), DdK (DdK), Struct (Comp, Prim), Dd)
import Sqel.Data.Sel (Sel (SelAuto, SelIndex, SelPath, SelSymbol, SelUnused))
import Sqel.SOP.Error (JoinSep, JoinSym, QuotedError, QuotedType, Unlines)

data CheckField =
  CheckField {
    path :: [Symbol],
    tpe :: Type
  }

type CheckFieldPrim :: [Symbol] -> Sel -> Type -> [CheckField]
type family CheckFieldPrim prefix sel t where
  CheckFieldPrim prefix ('SelSymbol name) t = '[ 'CheckField (prefix ++ '[name]) t]
  CheckFieldPrim _ ('SelPath path) t = '[ 'CheckField path t]
  CheckFieldPrim _ 'SelUnused _ = '[]
  CheckFieldPrim _ ('SelIndex _) _ = '[]
  CheckFieldPrim _ 'SelAuto t =
    TypeError ("Internal error: A field with type " <> t <> " was not automatically renamed.")

type CheckFieldsComp :: [Symbol] -> Sel -> Comp -> CompInc -> Type -> [DdK] -> [CheckField]
type family CheckFieldsComp prefix name c i t sub where
  CheckFieldsComp prefix _ _ 'Merge _ sub = CheckFieldsProd prefix sub
  CheckFieldsComp prefix ('SelSymbol name) 'Sum _ _ (_ : sub) = CheckFieldsProd (prefix ++ '[name]) sub
  CheckFieldsComp prefix ('SelSymbol name) _ _ _ sub = CheckFieldsProd (prefix ++ '[name]) sub
  CheckFieldsComp _ 'SelAuto _ _ t _ =
    TypeError ("Internal error: A composite column with type " <> t <> " was not automatically renamed.")

type CheckFieldsSub :: [Symbol] -> DdK -> [CheckField]
type family CheckFieldsSub prefix s where
  CheckFieldsSub prefix ('DdK sel _ t 'Prim) = CheckFieldPrim prefix sel t
  CheckFieldsSub prefix ('DdK sel _ t ('Comp _ c i d)) = CheckFieldsComp prefix sel c i t d
  CheckFieldsSub _ s = TypeError ("CheckFieldsSub: " <> s)

type CheckFieldsProd :: [Symbol] -> [DdK] -> [CheckField]
type family CheckFieldsProd prefix s where
  CheckFieldsProd _ '[] = '[]
  CheckFieldsProd prefix (s : ss) = CheckFieldsSub prefix s ++ CheckFieldsProd prefix ss
  CheckFieldsProd _ s = TypeError ("CheckFieldsProd: " <> s)

type CheckFields :: DdK -> [CheckField]
type family CheckFields s where
  CheckFields ('DdK ('SelSymbol name) _ t 'Prim) = '[ 'CheckField '[name] t]
  CheckFields ('DdK _ _ _ ('Comp _ _ _ sub)) = CheckFieldsProd '[] sub
  CheckFields s = TypeError ("CheckFields: " <> s)

-------------------------------------------------------------------------------------------------------

type family MatchField' (f1 :: CheckField) (f2 :: CheckField) :: Bool where
  MatchField' ('CheckField path _) ('CheckField path _) = 'True
  MatchField' _ _ = 'False

data MatchField :: CheckField -> CheckField -> Exp Bool
type instance Eval (MatchField f1 f2) = MatchField' f1 f2

data CheckQueryField :: [CheckField] -> CheckField -> Exp Bool
type instance Eval (CheckQueryField ts q) =
  All (Not <=< MatchField q) @@ ts

type family ShowField (field :: CheckField) :: ErrorMessage where
  ShowField ('CheckField path tpe) = "  " <> JoinSep "." path <> " [" <> tpe <> "]"

type family ShowFields (fields :: [CheckField]) :: [ErrorMessage] where
  ShowFields '[] = '[]
  ShowFields (field : fields) =
    ShowField field : ShowFields fields

data NoMatch :: [CheckField] -> CheckField -> Exp k
type instance Eval (NoMatch avail ('CheckField path tpe)) =
  TypeError (
    "The query column " <> QuotedError (JoinSym "." path) <> " with type " <> QuotedType tpe <>
    " does not correspond to a table column." %
    "Available fields:" %
    Unlines (ShowFields avail)
  )

type family CheckQueryFields (qs :: DdK) (s :: DdK) :: Maybe k where
  CheckQueryFields qs ts =
    FMap (NoMatch (CheckFields ts)) @@ (Find (CheckQueryField (CheckFields ts)) @@ CheckFields qs)

class PrintFields (s :: DdK) where
  printFields :: Dd s -> ()
  printFields _ = ()

instance (
    TypeError (Unlines (ShowFields (CheckFields s)) % s)
  ) => PrintFields s where
