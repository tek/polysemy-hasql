module Polysemy.Hasql.Table.QueryFields where

import Generics.SOP.Type.Metadata (FieldInfo(FieldInfo))

data T
data F

type family TypeEq a b where
  TypeEq a a = T
  TypeEq a b = F

class MatchingFieldName s q t n ft queryFields queryTypes fieldNames fieldTypes where

instance
  ∀ t n ft queryFields queryTypes fieldNames fieldTypes .
  (QueryFields queryFields queryTypes fieldNames fieldTypes, t ~ ft) =>
  MatchingFieldName T n t n ft queryFields queryTypes fieldNames fieldTypes
  where

instance
  ∀ q t n ft queryFields queryTypes fieldNames fieldTypes .
  QueryFields (q : queryFields) (t : queryTypes) fieldNames fieldTypes =>
  MatchingFieldName F q t n ft queryFields queryTypes fieldNames fieldTypes
  where

-- |Proof that the symbols in @queryFields@ are contained in the 'FieldInfo' values of @fieldNames@.
-- @fieldNames@ and @fieldTypes@ represent the names and types of the fields of a record data type.
-- @queryFields@ is a list of symbols that is specified at the call site, representing the intended fields that should
-- be queried in the database.
--
-- The proof is made based on whether the current heads of the lists @queryFields@ and @fieldNames@ contain the same
-- symbol.
--
-- The auxiliary class 'MatchingFieldName' is necessary to disambiguate the two cases, since ghc cannot decide based on
-- simple absence of an equality constraint, given that the instances have the same type.
-- The type family 'TypeEq' is used to select the correct instance, making their types clearly different.
class QueryFields queryFields queryTypes fieldNames fieldTypes where

instance QueryFields '[] '[] fieldNames fieldTypes where

instance (
    TypeEq q n ~ s,
    MatchingFieldName s ('FieldInfo q) t ('FieldInfo n) ft queryFields queryTypes fieldNames fieldTypes
  ) =>
  QueryFields (('FieldInfo q) : queryFields) (t : queryTypes) (('FieldInfo n) : fieldNames) (ft : fieldTypes)
  where
