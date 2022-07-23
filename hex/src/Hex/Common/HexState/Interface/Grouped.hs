module Hex.Common.HexState.Interface.Grouped where

import Hexlude

-- | For groups that do have an associated scope, the type of that group.
data ScopedGroupType
  = LocalStructureScopeGroup ChangeGroupTrigger
  | ExplicitBoxScopeGroup
  deriving stock (Show, Eq, Generic)

data HexGroupType
  = LocalStructureGroupType
  | ExplicitBoxGroupType
  | NonScopeGroupType
  deriving stock (Show, Generic)

data ChangeGroupTrigger
  = ChangeGroupCharTrigger
  | ChangeGroupCSTrigger
  deriving stock (Show, Eq, Generic)

data ScopeFlag
  = GlobalScope
  | LocalScope
  deriving stock (Show, Eq, Generic)
