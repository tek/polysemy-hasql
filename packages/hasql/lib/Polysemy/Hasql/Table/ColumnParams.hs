module Polysemy.Hasql.Table.ColumnParams where

import Polysemy.Db.Data.Column (Auto, ForeignKey, NewtypePrim, Prim, PrimaryKey, Sum, Unique)
import Polysemy.Db.Data.ColumnParams (ColumnParams(..))

class ExplicitColumnParams r where
  explicitColumnParams :: ColumnParams

instance {-# overlappable #-} ExplicitColumnParams a where
  explicitColumnParams =
    def

instance ExplicitColumnParams Unique where
  explicitColumnParams =
    def { unique = True }

instance ExplicitColumnParams PrimaryKey where
  explicitColumnParams =
    def { primaryKey = True }

instance ExplicitColumnParams Auto where
  explicitColumnParams =
    def

instance {-# overlappable #-} ExplicitColumnParams a => ExplicitColumnParams (Sum a) where
  explicitColumnParams =
    explicitColumnParams @a

instance {-# overlappable #-} ExplicitColumnParams a => ExplicitColumnParams (Prim a) where
  explicitColumnParams =
    explicitColumnParams @a

instance {-# overlappable #-} ExplicitColumnParams a => ExplicitColumnParams (NewtypePrim a) where
  explicitColumnParams =
    explicitColumnParams @a

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
