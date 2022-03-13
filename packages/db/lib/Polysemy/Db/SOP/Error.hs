module Polysemy.Db.SOP.Error where

import Fcf (Eval, Exp, UnList, type (@@))
import Type.Errors (ErrorMessage (Text))
import Type.Errors.Pretty (type (%), type (<>))

type family MessageWithType message d :: ErrorMessage where
  MessageWithType message d =
    'Text message <> 'Text ": " <> 'ShowType d

type family ErrorWithType message d :: k where
  ErrorWithType message d =
    TypeError ('Text message <> 'Text ": " <> 'ShowType d)

type family ErrorWithType2 part1 d part2 d' :: k where
  ErrorWithType2 part1 d part2 d' =
    TypeError ('Text part1 <> 'Text " " <> 'ShowType d <> 'Text " " <> 'Text part2 <> 'Text " " <> 'ShowType d')

type family JoinError (sep :: ErrorMessage) (ns :: [ErrorMessage]) :: ErrorMessage where
  JoinError _ '[] = 'Text "<empty>"
  JoinError sep (n : n1 : ns) = n <> sep <> JoinError sep (n1 : ns)
  JoinError _ '[n] = n

type family JoinComma (ns :: [ErrorMessage]) :: ErrorMessage where
  JoinComma ns = JoinError ('Text ", ") ns

type family JoinCommaSym (ns :: [Symbol]) :: ErrorMessage where
  JoinCommaSym (n : n1 : ns) = 'Text n <> ", " <> JoinCommaSym (n1 : ns)
  JoinCommaSym '[n] = 'Text n

type family QuotedError (msg :: ErrorMessage) :: ErrorMessage where
  QuotedError err = "'" <> err <> "'"

type family Quoted (s :: Symbol) :: ErrorMessage where
  Quoted s = QuotedError ('Text s)

type family QuotedType (t :: Type) :: ErrorMessage where
  QuotedType t = QuotedError ('ShowType t)

data LineBreak :: ErrorMessage -> ErrorMessage -> Exp ErrorMessage
type instance Eval (LineBreak l r) = l % r

type family Unlines (fragments :: [ErrorMessage]) :: ErrorMessage where
  Unlines '[] =
    'Text ""
  Unlines (h : t) =
    UnList h LineBreak @@ t
