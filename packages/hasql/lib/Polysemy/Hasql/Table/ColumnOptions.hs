module Polysemy.Hasql.Table.ColumnOptions where

import Prelude hiding (Enum)

import Polysemy.Db.Data.Column (Auto, Enum, ForeignKey, NewtypePrim, Prim, PrimaryKey, Sum, Unique)
import Polysemy.Db.Data.ColumnOptions (ColumnOptions(..))

class ExplicitColumnOptions r where
  explicitColumnOptions :: ColumnOptions

instance {-# overlappable #-} ExplicitColumnOptions a where
  explicitColumnOptions =
    def

instance ExplicitColumnOptions Unique where
  explicitColumnOptions =
    def { unique = True }

instance ExplicitColumnOptions PrimaryKey where
  explicitColumnOptions =
    def { primaryKey = True }

instance ExplicitColumnOptions Auto where
  explicitColumnOptions =
    def

instance {-# overlappable #-} ExplicitColumnOptions a => ExplicitColumnOptions (Sum a) where
  explicitColumnOptions =
    explicitColumnOptions @a

instance {-# overlappable #-} ExplicitColumnOptions a => ExplicitColumnOptions (Prim a) where
  explicitColumnOptions =
    explicitColumnOptions @a

instance {-# overlappable #-} ExplicitColumnOptions a => ExplicitColumnOptions (NewtypePrim a) where
  explicitColumnOptions =
    explicitColumnOptions @a

instance {-# overlappable #-} ExplicitColumnOptions a => ExplicitColumnOptions (Enum a) where
  explicitColumnOptions =
    explicitColumnOptions @a

instance ExplicitColumnOptions (Prim ForeignKey) where
  explicitColumnOptions =
    def

class ImplicitColumnOptions d where
  implicitColumnOptions :: ColumnOptions

instance {-# overlappable #-} ImplicitColumnOptions d where
  implicitColumnOptions =
    def

instance ImplicitColumnOptions d => ImplicitColumnOptions (Maybe d) where
  implicitColumnOptions =
    implicitColumnOptions @d <> def { notNull = False }
