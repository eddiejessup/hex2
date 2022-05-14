module Hex.Common.HexState.Interface.Grouped where

import Hex.Stage.Parse.Interface.AST.Command qualified as AST
import Hexlude

-- | For groups that do have an associated scope, the type of that group.
data ScopedGroupType
  = LocalStructurScopeGroup AST.CommandTrigger
  | ExplicitBoxScopeGroup
  deriving stock (Show)
