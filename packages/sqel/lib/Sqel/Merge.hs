module Sqel.Merge where

import Sqel.Data.Dd (Dd (Dd), DdInc (DdMerge), DdStruct (DdComp, DdPrim))
import Sqel.Type (Merge)

merge :: Dd s -> Dd (Merge s)
merge = \case
  Dd sel mods (DdComp tsel c _ sub) ->
    Dd sel mods (DdComp tsel c DdMerge sub)
  Dd sel mods DdPrim ->
    Dd sel mods DdPrim
