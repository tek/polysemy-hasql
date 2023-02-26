module Sqel.Class.MatchView where

import Type.Errors (ErrorMessage, IfStuck, Pure)

import Sqel.Data.Dd (DdK)
import Sqel.Data.FieldPath (FieldPath (FieldPath), FieldPaths, PathEq, ShowFields)
import Sqel.SOP.Error (JoinSym, QuotedError, QuotedType, Unlines)

type family CheckAvail (fields :: [FieldPath]) :: Type where
  CheckAvail ('FieldPath _ t : _) = t

type family AvailableColumns (fields :: [FieldPath]) :: ErrorMessage where
  AvailableColumns fields =
    "The specified table contains these fields:" %
    Unlines (ShowFields fields)

type NoViewMatch :: Symbol -> [FieldPath] -> FieldPath -> ErrorMessage
type family NoViewMatch viewType avail path where
  NoViewMatch viewType avail ('FieldPath path tpe) =
    "The " <> viewType <> " column " <> QuotedError (JoinSym "." path) <> " with type " <> QuotedType tpe <>
    " does not correspond to a table column." %
    AvailableColumns avail

type family UnknownMsg (viewType :: Symbol) (field :: FieldPath) :: ErrorMessage where
  UnknownMsg viewType ('FieldPath path tpe) =
    "This " <> viewType <> " cannot determine whether the column " <> QuotedError (JoinSym "." path) <> " with type " <>
    QuotedType tpe %
    "corresponds to a table column."

type family PathConstraint (path :: [Symbol]) :: ErrorMessage where
  PathConstraint '[field] = "HasField " <> 'ShowType field
  PathConstraint path = "HasPath " <> path

type family AvailStuckMsg (viewType :: Symbol) (table :: DdK) (field :: FieldPath) :: ErrorMessage where
  AvailStuckMsg viewType table ('FieldPath path tpe) =
    UnknownMsg viewType ('FieldPath path tpe) %
    "This is likely due to the structure type " <> QuotedType table <> " being polymorphic." %
    "Try adding the constraint:" %
    "  " <> QuotedError (PathConstraint path <> " " <> tpe <> " " <> table)

type MatchStuckMsg :: Symbol -> [FieldPath] -> FieldPath -> ErrorMessage
type family MatchStuckMsg viewType avail path where
  MatchStuckMsg viewType avail field =
    UnknownMsg viewType field %
    AvailableColumns avail

type family MatchStuck (viewType :: Symbol) (field :: FieldPath) (avail :: [FieldPath]) :: k where
  MatchStuck viewType field avail =
    TypeError (MatchStuckMsg viewType avail field)

type family AvailStuck (viewType :: Symbol) (table :: DdK) (field :: FieldPath) :: k where
  AvailStuck viewType table field =
    TypeError (AvailStuckMsg viewType table field)

type CheckMatch :: Symbol -> DdK -> FieldPath -> [FieldPath] -> Bool -> Bool
type family CheckMatch viewType table vfield avail match where
  CheckMatch viewType table vfield avail match =
    IfStuck match (IfStuck (CheckAvail avail) (AvailStuck viewType table vfield) (Pure (MatchStuck viewType vfield avail))) (Pure match)

type MatchPath :: FieldPath -> FieldPath -> Bool -> Constraint
class MatchPath view avail match | view avail -> match where

instance (
    match ~ PathEq view avail
  ) => MatchPath view avail match where

type CheckPathMatch :: FieldPath -> [FieldPath] -> Bool -> Bool -> Constraint
class CheckPathMatch view avail match finalMatch | view avail match -> finalMatch where

instance (
    MatchViewPath view avail match
  ) => CheckPathMatch view avail 'False match where

instance CheckPathMatch view avail 'True 'True where

type MatchViewPath :: FieldPath -> [FieldPath] -> Bool -> Constraint
class MatchViewPath view avail match | view avail -> match where

instance (
    MatchPath view tfield match,
    CheckPathMatch view tfields match finalMatch
  ) => MatchViewPath view (tfield : tfields) finalMatch where

instance MatchViewPath view '[] 'False where

-------------------------------------------------------------------------------------------------------

type CheckViewPathError :: Symbol -> DdK -> FieldPath -> [FieldPath] -> [FieldPath] -> Bool -> Maybe ErrorMessage -> Constraint
class CheckViewPathError viewType table vfield vfields avail match error | viewType table vfield vfields avail match -> error where

instance (
    MatchViewPaths viewType table vfields avail error
  ) => CheckViewPathError viewType table vfield vfields avail 'True error where

instance (
    err ~ NoViewMatch viewType avail vfield
  ) => CheckViewPathError viewType table vfield vfields avail 'False ('Just err) where

type MatchViewPaths :: Symbol -> DdK -> [FieldPath] -> [FieldPath] -> Maybe ErrorMessage -> Constraint
class MatchViewPaths viewType table view avail error | viewType table view avail -> error where

instance MatchViewPaths viewType table '[] avail 'Nothing where

instance (
    MatchViewPath vfield avail match,
    CheckViewPathError viewType table vfield vfields avail (CheckMatch viewType table vfield avail match) finalError
  ) => MatchViewPaths viewType table (vfield : vfields) avail finalError where

type family MaybeError (msg :: Maybe ErrorMessage) :: Maybe k where
  MaybeError 'Nothing = 'Nothing
  MaybeError ('Just msg) = 'Just (TypeError msg)

type MatchView :: Symbol -> DdK -> DdK -> Maybe Void -> Constraint
class MatchView viewType view avail error | viewType view avail -> error where

instance (
    MatchViewPaths viewType table (FieldPaths view) (FieldPaths table) msg,
    error ~ MaybeError msg
  ) => MatchView viewType view table error where

type MatchQuery view table error =
  MatchView "query" view table error

type MatchProjection view table error =
  MatchView "projection" view table error

-------------------------------------------------------------------------------------------------------

class (
    MatchViewPath ('FieldPath path t) (FieldPaths table) 'True
  ) => HasPath path t table where

instance (
    MatchViewPath ('FieldPath path t) (FieldPaths table) 'True
  ) => HasPath path t table where

class (
    MatchViewPath ('FieldPath '[path] t) (FieldPaths table) 'True
  ) => HasField path t table where

instance (
    MatchViewPath ('FieldPath '[path] t) (FieldPaths table) 'True
  ) => HasField path t table where
