module Polysemy.Hasql.Queue.Data.Queued where

import Sqel.Comp (CompName (compName))
import Sqel.Data.Sel (MkSel (mkSel), Sel (SelType), SelPrefix (NoPrefix))

data Queued t a =
  Queued {
    queue_created :: t,
    queue_payload :: a
  }
  deriving stock (Eq, Show, Generic)

instance (
    CompName d ('SelType prefix inner),
    name ~ AppendSymbol "Queued" inner,
    sel ~ 'SelType 'NoPrefix name,
    MkSel sel
  ) => CompName (Queued t d) sel where
  compName = mkSel
