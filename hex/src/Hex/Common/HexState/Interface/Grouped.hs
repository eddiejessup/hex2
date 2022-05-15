module Hex.Common.HexState.Interface.Grouped where

import Hexlude

-- | For groups that do have an associated scope, the type of that group.
data ScopedGroupType
  = LocalStructureScopeGroup LocalStructureTrigger
  | ExplicitBoxScopeGroup
  deriving stock (Show)

data LocalStructureTrigger = LocalStructureCharTrigger | LocalStructureCSTrigger
  deriving stock (Show, Eq, Generic)
