module Elea.Monad.Env.Data
(
  Data, empty,
  bindAt, matched, 
  matches, bindings
)
where

import Elea.Prelude
import Elea.Term
import Elea.Index
import Elea.Monad.Env ()
import qualified Elea.Monad.Env.Class as Env
import qualified Elea.Type as Type

data Data
  = Data  { _dbBinds :: [Bind]
          , _dbMatches :: [(Term, Term)] }
          
mkLabels [ ''Data ]

empty :: Data
empty = Data mempty mempty

matches :: Data -> [(Term, Term)]
matches = get dbMatches

bindings :: Data -> [Bind]
bindings = get dbBinds

bindAt :: Index -> Bind -> Data -> Data
bindAt at b = id
  . modify dbMatches (map (liftAt (enum at) *** liftAt (enum at)))
  . modify dbBinds (insertAt (enum at) b)

matched :: Term -> Term -> Data -> Data
matched t1 t2 = modify dbMatches (++ [(t1, t2)])

