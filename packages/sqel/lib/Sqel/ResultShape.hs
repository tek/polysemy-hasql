module Sqel.ResultShape where

import Data.Vector (Vector)
import Hasql.Decoders (Result, Row, noResult, rowList, rowMaybe, rowVector, singleRow)

class ResultShape d r | r -> d where
  resultShape :: Row d -> Result r

instance ResultShape d (Vector d) where
  resultShape =
    rowVector

instance ResultShape d [d] where
  resultShape =
    rowList

instance ResultShape d (Maybe d) where
  resultShape =
    rowMaybe

instance ResultShape () () where
  resultShape =
    const noResult

instance ResultShape a (Identity a) where
  resultShape =
    fmap Identity . singleRow
