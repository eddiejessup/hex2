module Hex.Common.HexState.Impl.Group where

import Hex.Stage.Parse.Interface.AST.Command qualified as H.Par.AST
import Hexlude
import Hex.Common.HexState.Impl.Scope (Scope)

-- | A group is a weaker idea than a scope.
-- Some groups introduce a scope, but some don't.
data HexGroup
  = ScopeGroup GroupScope
  | NonScopeGroup
  deriving stock (Show, Generic)

-- | For groups that do have an associated scope, the type of that group.
data GroupScopeType
  = LocalStructureGroupScope H.Par.AST.CommandTrigger
  | ExplicitBoxGroupScope
  deriving stock (Show)

-- | A scope, along with a tag representing the type of group that
-- introduced the scope.
data GroupScope = GroupScope {scgScope :: Scope, scgType :: GroupScopeType}
  deriving stock (Show, Generic)

-- From a group, get the associated scope, if any.
groupScopeATraversal :: AffineTraversal' HexGroup Scope
groupScopeATraversal = _Typed @GroupScope % typed @Scope

-- | From a list of groups, get all associated scopes.
groupListScopeTraversal :: Traversal' [HexGroup] Scope
groupListScopeTraversal = traversed % groupScopeATraversal
