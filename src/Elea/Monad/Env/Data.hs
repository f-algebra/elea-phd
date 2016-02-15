module Elea.Monad.Env.Data
(
  Data, empty,
  bindAt, matched, 
  matches, bindings,
  rewrites, addRewrite, forgetRewrites,
  history, forgetMatches,
  direction,
  disableFlag,
  clearContext, augmentContext, applyContext,
  traceStepsFlag,
  stepName,
)
where

import Elea.Prelude
import Elea.Term
import Elea.Term.Index
import Elea.Monad.Env ()
import Elea.Monad.Direction ( Direction )
import qualified Elea.Monad.History as History
import qualified Elea.Term.Ext as Term
import qualified Elea.Term.Index as Indices
import qualified Elea.Monad.Env.Class as Env
import qualified Elea.Monad.Direction as Direction
import qualified Elea.Transform.Names as Step
import qualified Elea.Type as Type

data Data
  = Data  { _dbBinds :: ![Bind]
          , _dbMatches :: ![Match]
          , _dbRewrites :: ![(Tag, Term, Term)] 
          , _dbHistory :: !History.Repr
          , _dbDirection :: !Direction
          , _flagDisable :: !Bool
          , _dbTermContext :: !(Term -> Term)
          , _flagTraceSteps :: !Bool
          , _dbStepName :: !Step.Name }
          
mkLabels [ ''Data ]

instance Empty Data where
  empty = Data mempty mempty mempty empty Direction.Inc False id False empty

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
matched m 
  | isVar (Term.matchedTerm m) = 
    modify dbMatches (map apply)
  | otherwise = 
    modify dbMatches (++ [m])
  where
  Var x _ = Term.matchedTerm m
  
  apply (Match cse_t ps n) = 
    Match cse_t' ps n
    where
    cse_t' = Indices.replaceAt x (Term.matchedTo m) cse_t
          
forgetMatches :: (Match -> Bool) -> Data -> Data
forgetMatches when = 
  modify dbMatches (filter when)

rewrites :: Data -> [(Tag, Term, Term)]
rewrites = get dbRewrites

addRewrite :: Tag -> Term -> Term -> Data -> Data
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

clearContext :: Data -> Data
clearContext = set dbTermContext id

augmentContext :: (Term -> Term) -> Data -> Data
augmentContext context = modify dbTermContext (. context)

applyContext :: Term -> Data -> Term
applyContext gap_term = ($ gap_term) . get dbTermContext

traceStepsFlag :: Data :-> Bool
traceStepsFlag = flagTraceSteps

stepName :: Data :-> Step.Name
stepName = dbStepName
