module Polysemy.Hasql.Table.ColumnOptions where

import Polysemy.Db.Data.Rep (Rep)
import Prelude hiding (Enum)

import Polysemy.Db.Data.Rep (Auto, ForeignKey, PrimaryKey, Unique)
import Polysemy.Db.Data.ColumnOptions (ColumnOptions(..))

class ExplicitColumnOptions (r :: Type) where
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

instance ExplicitColumnOptions ForeignKey where
  explicitColumnOptions =
    def

class ImplicitColumnOptions (d :: Type) where
  implicitColumnOptions :: ColumnOptions

instance {-# overlappable #-} ImplicitColumnOptions d where
  implicitColumnOptions =
    def

instance ImplicitColumnOptions d => ImplicitColumnOptions (Maybe d) where
  implicitColumnOptions =
    implicitColumnOptions @d <> def { notNull = False }

type family RepToList (rep :: Type) :: [Type] where
  RepToList (Rep rep) = rep
  RepToList Auto = '[]
  RepToList rep = '[rep]

class RepOptions (reps :: [Type]) where
  repOptions :: ColumnOptions

instance RepOptions '[] where
  repOptions =
    mempty

instance (
    ExplicitColumnOptions rep,
    RepOptions reps
  ) => RepOptions (rep : reps) where
  repOptions =
    explicitColumnOptions @rep <> repOptions @reps
