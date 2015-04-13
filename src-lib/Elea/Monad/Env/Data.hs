module Elea.Monad.Env.Data
(
  Data, empty,
  bindAt, matched, 
  matches, bindings,
  rewrites, addRewrite,
  codes, addCode
)
where

import Elea.Prelude
import Elea.Term
import Elea.Index
import Elea.Embed ( Code )
import Elea.Monad.Env ()
import qualified Elea.Index as Indices
import qualified Elea.Monad.Env.Class as Env
import qualified Elea.Monad.Rewrite as Rewrite
import qualified Elea.Type as Type

data Data
  = Data  { _dbBinds :: [Bind]
          , _dbMatches :: [(Term, Term)]
          , _dbRewrites :: [(Tag, Term, Index)] 
          , _dbCodes :: [Code] }
          
mkLabels [ ''Data ]

empty :: Data
empty = Data mempty mempty mempty mempty

matches :: Data -> [(Term, Term)]
matches = get dbMatches

bindings :: Data -> [Bind]
bindings = get dbBinds

bindAt :: Index -> Bind -> Data -> Data
bindAt at b = id
  . modify dbMatches (map (liftAt (enum at) *** liftAt (enum at)))
  . modify dbBinds (insertAt (enum at) b)

matched :: Term -> Term -> Data -> Data
matched t1 t2 = modify dbMatches ((++ [(t1, t2)]) . replace)
  where
  replace :: [(Term, Term)] -> [(Term, Term)]
  replace | Var x <- t1 = map (Indices.replaceAt x t2)
          | otherwise = id

rewrites :: Data -> [(Tag, Term, Index)]
rewrites = get dbRewrites

addRewrite :: Tag -> Term -> Index -> Data -> Data
addRewrite a t x = modify dbRewrites ((a, t, x) :) 

codes :: Data -> [Code]
codes = get dbCodes

addCode :: Code -> Data -> Data
addCode code = modify dbCodes (code :)

