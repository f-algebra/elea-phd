module Elea.Transform.Names
(
  Name (..)
)
where

import Elea.Prelude

data Name
  = CaseOfCon
  | Beta
  | TraverseMatch
  | TraverseVarBranch
  | TraverseBranch
  | TraverseApp
  | Unfold
  | UnfoldCase
  | ConstArg
  | FiniteCaseFix
  | FloatVarMatch
  | ExpressCon
  | ExpressMatch
  | FixFixFusion
  | RepArgFusion
  | FreeArgFusion
  | MatchFixFusion
  | CaseFun
  | Fusion
  | RewriteEq
  | RewriteLeq
  | Forall
  | LFP
  | CaseSplit
  deriving ( Enum, Show )

