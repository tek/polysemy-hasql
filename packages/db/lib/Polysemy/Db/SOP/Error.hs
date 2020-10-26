module Polysemy.Db.SOP.Error where

import Type.Errors (ErrorMessage(Text, ShowType), TypeError)
import Type.Errors.Pretty (type (<>))

type family MessageWithType message d :: ErrorMessage where
  MessageWithType message d =
    'Text message <> 'Text ": " <> 'ShowType d

type family ErrorWithType message d :: k where
  ErrorWithType message d =
    TypeError ('Text message <> 'Text ": " <> 'ShowType d)

type family ErrorWithType2 part1 d part2 d' :: k where
  ErrorWithType2 part1 d part2 d' =
    TypeError ('Text part1 <> 'Text " " <> 'ShowType d <> 'Text " " <> 'Text part2 <> 'Text " " <> 'ShowType d')

type family JoinComma (ns :: [Symbol]) :: ErrorMessage where
  JoinComma (n : n1 : ns) = 'Text n <> ", " <> JoinComma (n1 : ns)
  JoinComma '[n] = 'Text n
