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
  | ExpressCon
  | ExpressMatch
  | FixFixFusion
  | RepArgFusion
  | FreeArgFusion
  deriving Enum

