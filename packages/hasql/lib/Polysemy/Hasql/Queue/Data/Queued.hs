module Polysemy.Hasql.Queue.Data.Queued where

import Sqel.Comp (CompName (compName))
import Sqel.Data.Sel (MkTSel (mkTSel), SelPrefix (NoPrefix), TSel (TSel))

data Queued t a =
  Queued {
    queue_created :: t,
    queue_payload :: a
  }
  deriving stock (Eq, Show, Generic)

instance (
    CompName d ('TSel prefix inner),
    name ~ AppendSymbol "Queued" inner,
    sel ~ 'TSel 'NoPrefix name,
    MkTSel sel
  ) => CompName (Queued t d) sel where
  compName = mkTSel
