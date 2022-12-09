module Polysemy.Hasql.Queue.Data.Queued where

import Sqel.Data.Dd (DbTypeName (dbTypeName))
import Sqel.Data.Sel (MkSel (mkSel), Sel (SelSymbol))

data Queued t a =
  Queued {
    queue_created :: t,
    queue_payload :: a
  }
  deriving stock (Eq, Show, Generic)

instance (
    DbTypeName d inner,
    name ~ AppendSymbol "Queued" inner,
    MkSel ('SelSymbol name)
  ) => DbTypeName (Queued t d) name where
    dbTypeName = mkSel
