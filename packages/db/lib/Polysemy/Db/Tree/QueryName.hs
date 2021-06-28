{-# options_ghc -Wno-all -Wno-redundant-constraints #-}
module Polysemy.Db.Tree.QueryName where

import Fcf (ConstFn, Eval, Exp, Pure1, UnEither, Zip, type (@@))
import Fcf.Class.Foldable (ConcatMap)
import Generics.SOP.GGP (GCode)
import Type.Errors (ErrorMessage (ShowType), IfStuck, Pure, ShowTypeQuoted, TypeError)
import Type.Errors.Pretty (type (%), type (<>))

import Polysemy.Db.Data.FieldId (FieldId (NamedField, NumberedField))
import Polysemy.Db.Data.Rep (Prim, PrimQuery)
import Polysemy.Db.SOP.Constraint (DataNameF)
import Polysemy.Db.SOP.FieldNames (FieldIds)
import Polysemy.Db.Tree.Data.Effect (ADT, Newtype)

type Result =
  Either Symbol Symbol

type ErrorIntro q =
  "Cannot determine the name of the field to which the primitive query type " <> ShowTypeQuoted q <> " refers."

type PrimNameNotGeneric =
  'Left "has no Generic instance."

type family PrimNameField (fid :: FieldId) :: Result where
  PrimNameField ('NumberedField _ _) =
    'Left "is not a record."
  PrimNameField ('NamedField name) =
    'Right name

type family PrimNameProdNames (ds :: [FieldId]) :: Result where
  PrimNameProdNames '[field] =
    PrimNameField field
  PrimNameProdNames '[] =
    'Left "has no field of that type."
  PrimNameProdNames _ =
    'Left "has multiple fields of that type."

type family MatchQueryType' (q :: Type) (field :: (FieldId, Type)) :: [FieldId] where
  MatchQueryType' q '(id, q) = '[id]
  MatchQueryType' _ _ = '[]

data MatchQueryType (q :: Type) :: (FieldId, Type) -> Exp [FieldId]
type instance Eval (MatchQueryType q f) =
  MatchQueryType' q f

type family PrimNameProd (q :: Type) (fss :: [[FieldId]]) (dss :: [[*]]) :: Result where
  PrimNameProd q (fs : _) (ds : _) =
    PrimNameProdNames (ConcatMap (MatchQueryType q) @@ (Zip fs @@ ds))
  PrimNameProd _ _ _ =
    'Left "has no constructors."

type family PrimName (q :: Type) (d :: Type) :: Result where
  PrimName q d = IfStuck (GCode d) PrimNameNotGeneric (Pure (PrimNameProd q (FieldIds d) (GCode d)))

type family QueryName' (effs :: [Type]) (q :: Type) (d :: Type) :: Result where
  QueryName' (Prim : _) q d =
    PrimName q d
  QueryName' (Newtype nt inner : effs) nt d =
    UnEither (ConstFn (QueryName' effs inner d)) (Pure1 'Right) @@ PrimName nt d
  QueryName' (ADT _ _ : _) q _ =
    'Right (DataNameF q)
  QueryName' (_ : effs) q d =
    QueryName' effs q d
  QueryName' '[] q _ =
    TypeError (ErrorIntro q % "Its effect stack contains no clues as to the nature of the query type.")

type family FindPrimQuery (effs :: [Type]) :: Maybe Symbol where
  FindPrimQuery '[] = 'Nothing
  FindPrimQuery (PrimQuery name : _) = 'Just name
  FindPrimQuery (_ : effs) = FindPrimQuery effs

type family PrimQueryOrResolve (prim :: Maybe Symbol) (effs :: [Type]) (q :: Type) (d :: Type) :: Result where
  PrimQueryOrResolve 'Nothing effs q d =
    QueryName' effs q d
  PrimQueryOrResolve ('Just name) _ _ _ =
    'Right name

data QueryNameError :: Type -> Type -> Symbol -> Exp k
type instance Eval (QueryNameError q d msg) =
  TypeError (ErrorIntro q % "The data type " <> ShowTypeQuoted d <> " " <> msg)

type family QueryName (effs :: [Type]) (q :: Type) (d :: Type) :: Symbol where
  QueryName effs q d =
    UnEither (QueryNameError q d) Pure @@ PrimQueryOrResolve (FindPrimQuery effs) effs q d
