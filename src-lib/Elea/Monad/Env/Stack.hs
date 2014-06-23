module Elea.Monad.Env.Stack 
(
  Stack,
  empty,
  bindAt,
  matched,
  bindings,
  matches,
)
where

import Elea.Prelude
import Elea.Index
import Elea.Term
import qualified Elea.Monad.Env as Env

data Stack 
  = Stack   { _stackBinds :: [Bind] 
            , _stackMatches :: [(Term, Term)] }
            
            
mkLabels [ ''Stack ]

empty :: Stack 
empty = Stack [] []

bindAt :: Index -> Bind -> Stack -> Stack
bindAt at b = id
  . modify stackBinds (insertAt (enum at) b)
  . modify stackMatches (liftAt at)

matched :: Term -> Term -> Stack -> Stack
matched t t' = modify stackMatches (++ [(t, t')])

bindings :: Stack -> [Bind]
bindings = get stackBinds

matches :: Stack -> [(Term, Term)]
matches = get stackMatches

