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
  | MatchVar
  | ConstArg
  | FiniteCaseFix
  | FloatVarMatch
  | ExpressCon
  | ExpressMatch
  | FixFixFusion
  | ConFusion
  | RepArgFusion
  | FreeArgFusion
  | MatchFixFusion
  | AccFusion
  | IdFix
  | CaseFun
  | Fusion
  | LeftTrans
  | RightTrans
  | Forall
  | LFP
  | CaseSplit
  | FoldDiscovery
  | AbsurdEnv
  deriving ( Enum, Show )

