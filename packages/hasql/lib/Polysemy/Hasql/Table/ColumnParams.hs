module Polysemy.Hasql.Table.ColumnParams where

import Polysemy.Db.Data.Column (Auto, ForeignKey, Prim, PrimaryKey, Sum, Unique)
import Polysemy.Db.Data.ColumnParams (ColumnParams(..))
import Polysemy.Hasql.Data.ColumnType (AutoInternal)

class ExplicitColumnParams r where
  explicitColumnParams :: ColumnParams

instance ExplicitColumnParams (Prim Unique) where
  explicitColumnParams =
    def { unique = True }

instance ExplicitColumnParams (Prim PrimaryKey) where
  explicitColumnParams =
    def { primaryKey = True }

instance ExplicitColumnParams (Prim Auto) where
  explicitColumnParams =
    def

instance ExplicitColumnParams (Sum a) where
  explicitColumnParams =
    def

instance ExplicitColumnParams Auto where
  explicitColumnParams =
    def

instance ExplicitColumnParams AutoInternal where
  explicitColumnParams =
    def

instance ExplicitColumnParams (Prim ForeignKey) where
  explicitColumnParams =
    def

class ImplicitColumnParams d where
  implicitColumnParams :: ColumnParams

instance {-# overlappable #-} ImplicitColumnParams d where
  implicitColumnParams =
    def

instance ImplicitColumnParams d => ImplicitColumnParams (Maybe d) where
  implicitColumnParams =
    implicitColumnParams @d <> def { notNull = False }
