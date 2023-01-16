module Sqel.Data.FieldPath where

import Prelude hiding (type (@@))
import Type.Errors (ErrorMessage)

import Sqel.Data.Dd (Comp (Sum), CompInc (Merge), Dd, DdK (DdK), Struct (Comp, Prim))
import Sqel.Data.Sel (Sel (SelAuto, SelIndex, SelPath, SelSymbol, SelUnused))
import Sqel.SOP.Error (JoinSep, QuotedType, Unlines)

data FieldPath =
  FieldPath {
    path :: [Symbol],
    tpe :: Type
  }

type FieldPathPrim :: [Symbol] -> Sel -> Type -> [FieldPath]
type family FieldPathPrim prefix sel t where
  FieldPathPrim prefix ('SelSymbol name) t = '[ 'FieldPath (prefix ++ '[name]) t]
  FieldPathPrim _ ('SelPath path) t = '[ 'FieldPath path t]
  FieldPathPrim _ 'SelUnused _ = '[]
  FieldPathPrim _ ('SelIndex _ _) _ = '[]
  FieldPathPrim _ 'SelAuto t =
    TypeError ("Internal error: A field with type " <> t <> " was not automatically renamed.")

type FieldPathsComp :: [Symbol] -> Sel -> Comp -> CompInc -> Type -> [DdK] -> [FieldPath]
type family FieldPathsComp prefix name c i t sub where
  FieldPathsComp prefix _ _ 'Merge _ sub = FieldPathsProd prefix sub
  FieldPathsComp prefix ('SelSymbol name) 'Sum _ _ (_ : sub) = FieldPathsProd (prefix ++ '[name]) sub
  FieldPathsComp prefix ('SelSymbol name) _ _ _ sub = FieldPathsProd (prefix ++ '[name]) sub
  FieldPathsComp _ 'SelAuto _ _ t _ =
    TypeError ("Internal error: A composite column with type " <> QuotedType t <> " was not automatically renamed.")

type FieldPathsSub :: [Symbol] -> DdK -> [FieldPath]
type family FieldPathsSub prefix s where
  FieldPathsSub prefix ('DdK sel _ t 'Prim) = FieldPathPrim prefix sel t
  FieldPathsSub prefix ('DdK sel _ t ('Comp _ c i d)) = FieldPathsComp prefix sel c i t d
  FieldPathsSub _ s = TypeError ("FieldPathsSub: " <> s)

type FieldPathsProd :: [Symbol] -> [DdK] -> [FieldPath]
type family FieldPathsProd prefix s where
  FieldPathsProd _ '[] = '[]
  FieldPathsProd prefix (s : ss) = FieldPathsSub prefix s ++ FieldPathsProd prefix ss
  FieldPathsProd _ s = TypeError ("FieldPathsProd: " <> s)

type FieldPaths :: DdK -> [FieldPath]
type family FieldPaths s where
  FieldPaths ('DdK ('SelSymbol name) _ t 'Prim) = '[ 'FieldPath '[name] t]
  FieldPaths ('DdK _ _ _ ('Comp _ _ _ sub)) = FieldPathsProd '[] sub
  FieldPaths s = TypeError ("FieldPaths: " <> s)

-------------------------------------------------------------------------------------------------------

type family PathEq (f1 :: FieldPath) (f2 :: FieldPath) :: Bool where
  PathEq ('FieldPath path _) ('FieldPath path _) = 'True
  PathEq _ _ = 'False

type family ShowField (field :: FieldPath) :: ErrorMessage where
  ShowField ('FieldPath path tpe) = "  " <> JoinSep "." path <> " [" <> tpe <> "]"

type family ShowFields (fields :: [FieldPath]) :: [ErrorMessage] where
  ShowFields '[] = '[]
  ShowFields (field : fields) =
    ShowField field : ShowFields fields

class PrintFields (s :: DdK) where
  printFields :: Dd s -> ()
  printFields _ = ()

instance (
    TypeError (Unlines (ShowFields (FieldPaths s)) % s)
  ) => PrintFields s where
