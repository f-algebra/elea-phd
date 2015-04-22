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
  | EqReduceCon
  | EqMatchCon
  | EqInduction
  | Unfold
  | UnfoldCase
  | FloatVarMatch
  | ExpressCon
  | ExpressMatch
  | FixFixFusion
  | RepArgFusion
  | FreeArgFusion
  | MatchFixFusion
  deriving ( Enum, Show )

