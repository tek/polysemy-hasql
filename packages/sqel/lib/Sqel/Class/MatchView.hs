module Sqel.Class.MatchView where

import Type.Errors (ErrorMessage)

import Sqel.Data.Dd (DdK)
import Sqel.Data.FieldPath (FieldPath (FieldPath), FieldPaths, PathEq, ShowFields)
import Sqel.SOP.Error (JoinSym, QuotedError, QuotedType, Unlines)

type NoViewMatch :: Symbol -> [FieldPath] -> FieldPath -> ErrorMessage
type family NoViewMatch viewType avail path where
  NoViewMatch viewType avail ('FieldPath path tpe) =
    "The " <> viewType <> " column " <> QuotedError (JoinSym "." path) <> " with type " <> QuotedType tpe <>
    " does not correspond to a table column." %
    "The specified table contains these fields:" %
    Unlines (ShowFields avail)

type MatchPath :: FieldPath -> FieldPath -> Bool -> Constraint
class MatchPath view table match | view table -> match where

instance (
    match ~ PathEq view table
  ) => MatchPath view table match where

type CheckPathMatch :: FieldPath -> [FieldPath] -> Bool -> Bool -> Constraint
class CheckPathMatch view table match finalMatch | view table match -> finalMatch where

instance (
    MatchViewPath view table match
  ) => CheckPathMatch view table 'False match where

instance CheckPathMatch view table 'True 'True where

type MatchViewPath :: FieldPath -> [FieldPath] -> Bool -> Constraint
class MatchViewPath view table match | view table -> match where

instance (
    MatchPath view tfield match,
    CheckPathMatch view tfields match finalMatch
  ) => MatchViewPath view (tfield : tfields) finalMatch where

instance MatchViewPath view '[] 'False where

-------------------------------------------------------------------------------------------------------

type CheckViewPathError :: Symbol -> FieldPath -> [FieldPath] -> [FieldPath] -> Bool -> Maybe ErrorMessage -> Constraint
class CheckViewPathError viewType vfield vfields table match error | viewType vfield vfields table match -> error where

instance (
    MatchViewPaths viewType vfields table error
  ) => CheckViewPathError viewType vfield vfields table 'True error where

instance (
    err ~ NoViewMatch viewType table vfield
  ) => CheckViewPathError viewType vfield vfields table 'False ('Just err) where

type MatchViewPaths :: Symbol -> [FieldPath] -> [FieldPath] -> Maybe ErrorMessage -> Constraint
class MatchViewPaths viewType view table error | viewType view table -> error where

instance MatchViewPaths viewType '[] table 'Nothing where

instance (
    MatchViewPath vfield table match,
    CheckViewPathError viewType vfield vfields table match finalError
  ) => MatchViewPaths viewType (vfield : vfields) table finalError where

type family MaybeError (msg :: Maybe ErrorMessage) :: Maybe k where
  MaybeError 'Nothing = 'Nothing
  MaybeError ('Just msg) = 'Just (TypeError msg)

type MatchView :: Symbol -> DdK -> DdK -> Maybe Void -> Constraint
class MatchView viewType view table error | viewType view table -> error where

-- TODO WhenStuck
instance (
    MatchViewPaths viewType (FieldPaths view) (FieldPaths table) msg,
    error ~ MaybeError msg
  ) => MatchView viewType view table error where

type MatchQuery view table error =
  MatchView "query" view table error

type MatchProjection view table error =
  MatchView "projection" view table error

-------------------------------------------------------------------------------------------------------

class (
    MatchViewPath ('FieldPath path t) (FieldPaths table) 'True
  ) => HasColumn path t table where

instance (
    MatchViewPath ('FieldPath path t) (FieldPaths table) 'True
  ) => HasColumn path t table where
