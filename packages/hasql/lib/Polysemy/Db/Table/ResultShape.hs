module Polysemy.Db.Table.ResultShape where

import Data.Vector (Vector)
import Hasql.Decoders (Result, Row, noResult, rowList, rowMaybe, rowVector)
import Prelude hiding (All, Generic)

class ResultShape a b where
  resultShape :: Row a -> Result b

instance ResultShape a (Vector a) where
  resultShape =
    rowVector

instance ResultShape a [a] where
  resultShape =
    rowList

instance ResultShape a (Maybe a) where
  resultShape =
    rowMaybe

instance ResultShape a () where
  resultShape =
    const noResult
