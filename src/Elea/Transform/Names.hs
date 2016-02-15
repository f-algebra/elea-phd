module Elea.Transform.Names
(
  Name (..)
)
where

import Elea.Prelude

data Name
  = TopLevel

  -- Evaluator steps
  | CaseOfCon
  | NormaliseApp
  | Strictness
  | BetaReduce
  | CaseAsFun
  | CaseAsArg
  | CommuteCases
  | TraverseMatch
  | TraverseBranches
  | TraverseApp
  | TraverseLam
  | TraverseFix

  -- Simplifier steps
  | ConstArgFusion
  | LambdaBranch
  | IdentityCase
  | SubtermFission
  | Unfold
  | UnfoldCase

  -- Prover steps
  | LeqReflexive
  | BotLeq
  | LeqUnit
  | DoubleNegation
  | ForAll
  | UnusedForAll
  | LeqLeftTrans
  | LeqRightTrans
  | CaseSplitInc
  | CaseSplitDec
  | LeqConstructor
  | LeastFixedPoint
  | LeqProductive
  | CaseOfLeq
  | AbsurdBranch

  -- Rewriter steps
  | RewritePattern
  | Rewrite
  | ConstructorFission
  | CommuteConstraint
  | AccumulationFission
  | CaseOfMatchVar
  | UnfoldFiniteFix
  | IdentityFission

  -- Fusion steps
  | FixFixFusion
  | FixConFusion
  | RepeatedArgFusion
  | FreeVarFusion
  | AssertionFusion
  | AccumulatorFusion
  | FoldDiscovery
  deriving ( Enum, Eq, Ord, Show )

instance Empty Name where
  empty = TopLevel

instance PrintfArg Name where
  formatArg = formatArg . show


