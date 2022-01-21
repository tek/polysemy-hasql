module Polysemy.Hasql.Where.FieldSelector where

import Polysemy.Hasql.Data.DbType (Selector (Selector))
import Polysemy.Hasql.Data.SqlCode (SqlCode)
import Polysemy.Hasql.Tree.Table (DbQueryRoot, TableRoot)
import Polysemy.Hasql.Where (ReifySegments, reifyPath)
import Polysemy.Hasql.Where.Cond (MatchTable, PrimCond (PrimCond), QCond (SimpleCond, SumPrimCond))

class CondSelector cond where
  condSelector :: SqlCode

instance (
    ReifySegments path
  ) => CondSelector ('SimpleCond ('PrimCond q d path)) where
    condSelector =
      reifyPath @path

instance (
    ReifySegments path
  ) => CondSelector ('SumPrimCond q d (path : otherPaths)) where
    condSelector =
      reifyPath @path

class FieldSelector a q rep d where
  fieldSelector :: Selector

instance (
    TableRoot rep d dTree,
    DbQueryRoot a q d qTree,
    '[cond] ~ MatchTable qTree dTree,
    CondSelector cond
  ) => FieldSelector a q rep d where
    fieldSelector =
      Selector (condSelector @cond)
