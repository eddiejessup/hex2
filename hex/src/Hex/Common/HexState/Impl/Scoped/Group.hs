module Hex.Common.HexState.Impl.Scoped.Group where

import Formatting qualified as F
import Hex.Common.HexState.Impl.Scoped.Scope (Scope)
import Hex.Stage.Parse.Interface.AST.Command qualified as H.Par.AST
import Hexlude

-- | A group is a weaker idea than a scope.
-- Some groups introduce a scope, but some don't.
data HexGroup
  = ScopeGroup GroupScope
  | NonScopeGroup
  deriving stock (Show, Generic)

fmtGroupWithoutScopeDetails :: Fmt HexGroup
fmtGroupWithoutScopeDetails = F.later $ \case
  NonScopeGroup -> "Non-scope group"
  ScopeGroup scg -> F.bformat fmtGroupScopeWithoutScopeDetails scg

-- | A scope, along with a tag representing the type of group that
-- introduced the scope.
data GroupScope = GroupScope {scgScope :: Scope, scgType :: GroupScopeType}
  deriving stock (Show, Generic)

fmtGroupScopeWithoutScopeDetails :: Fmt GroupScope
fmtGroupScopeWithoutScopeDetails = F.accessed (.scgScope) F.shown

-- | For groups that do have an associated scope, the type of that group.
data GroupScopeType
  = LocalStructureGroupScope H.Par.AST.CommandTrigger
  | ExplicitBoxGroupScope
  deriving stock (Show)

-- From a group, get the associated scope, if any.
groupScopeATraversal :: AffineTraversal' HexGroup Scope
groupScopeATraversal = _Typed @GroupScope % typed @Scope

-- | From a list of groups, get all associated scopes.
groupListScopeTraversal :: Traversal' [HexGroup] Scope
groupListScopeTraversal = traversed % groupScopeATraversal
