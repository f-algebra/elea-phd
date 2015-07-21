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
  | IdFix
  | CaseFun
  | Fusion
  | LeftTrans
  | RightTrans
  | Forall
  | LFP
  | CaseSplit
  | FoldDiscovery
  deriving ( Enum, Show )

