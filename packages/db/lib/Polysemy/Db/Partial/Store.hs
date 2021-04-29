module Polysemy.Db.Partial.Store where

import Polysemy.Db.Data.PartialFields (PartialFields)

class UpdateStorePartial i (d :: *) u where

instance UpdateStorePartial i d (PartialFields d) where
