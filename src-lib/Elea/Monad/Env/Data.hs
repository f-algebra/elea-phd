module Elea.Monad.Env.Data
(
  Data, empty,
  bindAt, matched, 
  matches, bindings,
  rewrites, addRewrite, forgetRewrites,
  history, forgetMatches,
  direction,
  disableFlag
)
where

import Elea.Prelude
import Elea.Term
import Elea.Term.Index
import Elea.Monad.Env ()
import Elea.Monad.Direction ( Direction )
import qualified Elea.Monad.History as History
import qualified Elea.Term.Index as Indices
import qualified Elea.Monad.Env.Class as Env
import qualified Elea.Monad.Fusion as Fusion
import qualified Elea.Monad.Direction as Direction
import qualified Elea.Type as Type

data Data
  = Data  { _dbBinds :: [Bind]
          , _dbMatches :: [Match]
          , _dbRewrites :: [(Tag, Term, Index)] 
          , _dbHistory :: History.Repr
          , _dbDirection :: Direction
          , _flagDisable :: Bool }
          
mkLabels [ ''Data ]

empty :: Data
empty = Data mempty mempty mempty History.empty Direction.Inc False

matches :: Data -> [Match]
matches = get dbMatches

bindings :: Data -> [Bind]
bindings = get dbBinds

bindAt :: Index -> Bind -> Data -> Data
bindAt at b = id
  . liftRewritesAt (enum at)
  . modify dbMatches (map (liftAt (enum at)))
  . modify dbBinds (insertAt (enum at) b)

matched :: Match -> Data -> Data
matched m = modify dbMatches (++ [m])
          
forgetMatches :: (Match -> Bool) -> Data -> Data
forgetMatches when = 
  modify dbMatches (filter when)

rewrites :: Data -> [(Tag, Term, Index)]
rewrites = get dbRewrites

addRewrite :: Tag -> Term -> Index -> Data -> Data
addRewrite a t x = modify dbRewrites ((a, t, x) :) 

forgetRewrites :: Data -> Data
forgetRewrites = set dbRewrites []

history :: (Data :-> History.Repr)
history = dbHistory

liftRewritesAt :: Index -> Data -> Data
liftRewritesAt at = 
  modify dbRewrites (map liftR)
  where
  liftR (a, t, x) = (a, liftAt at t, liftAt at x)
  
direction :: (Data :-> Direction)
direction = dbDirection

disableFlag :: (Data :-> Bool)
disableFlag = flagDisable



