module Hex.Common.HexState.Impl.Scoped.GroupScopes where

import Formatting qualified as F
import Hex.Common.HexState.Impl.Scoped.Group
import Hex.Common.HexState.Impl.Scoped.Scope (Scope, newGlobalScope)
import Hex.Common.HexState.Impl.Scoped.Scope qualified as Scope
import Hex.Common.HexState.Interface.Grouped qualified as Group
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hexlude
import Optics.Core qualified as O

-- | Collection of relevant scopes and groups.
-- We always have a global scope,
-- and possibly local groups, which may introduce scopes.
-- The left-most group is the innermost group.
data GroupScopes = GroupScopes
  { globalScope :: Scope,
    groups :: [HexGroup]
  }
  deriving stock (Show, Generic)

newGroupScopes :: MonadIO m => m GroupScopes
newGroupScopes = do
  globalScope <- newGlobalScope
  pure GroupScopes {globalScope, groups = []}

pushGroup :: Maybe Group.ScopedGroupType -> GroupScopes -> GroupScopes
pushGroup groupType =
  #groups %~ O.cons (newGroup groupType)

popGroup :: GroupScopes -> Maybe (HexGroup, GroupScopes)
popGroup groupScopes =
  groupScopes ^. #groups % to O.uncons <&> \(poppedGroup, postPopGroups) ->
    (poppedGroup, groupScopes & #groups .~ postPopGroups)

fmtGroupScopes :: Fmt GroupScopes
fmtGroupScopes =
  mconcat
    [ fmtListWithHeading "Groups" (.groups) fmtGroupWithoutScopeDetails,
      F.accessed seenLocalScope Scope.fmtScope |%| "\n"
    ]

-- The scope seen from the innermost scope, folding together all variables.
seenLocalScope :: GroupScopes -> Scope
seenLocalScope = foldOf scopesTraversal

-- | Get all scopes except global scope.
localScopesTraversal :: Traversal' GroupScopes Scope
localScopesTraversal = #groups % groupListScopeTraversal

-- | Get all scopes, including global scope.
scopesTraversal :: Traversal' GroupScopes Scope
scopesTraversal =
  -- This means we visit the local-scopes, then last we visit the global-scope.
  -- Note that this means it's a non-empty traversal, because we always have the
  -- global scope.
  localScopesTraversal `O.adjoin` #globalScope

-- | For some getter on a scope that might fail, try each scope in turn,
-- including global scope. If any succeeds, return its result.
scopedLookup :: (Scope -> Maybe v) -> GroupScopes -> Maybe v
scopedLookup f c =
  -- What's this doing?
  -- - Take the scopes traversal, but treat it as a fold, that's all we need here.
  -- - Map each element to 'Maybe a', but wrap that in 'First' to get the monoid instance where we take the first 'Just'.
  -- - Fold over the result.
  -- - Unwrap the 'First' value to get the underlying 'Maybe'.
  getFirst $ O.foldMapOf scopesTraversal (First . f) c

localMostScopeLens :: Lens' GroupScopes Scope
localMostScopeLens = lens getter setter
  where
    getter :: GroupScopes -> Scope
    getter grpScopes = case headOf scopesTraversal grpScopes of
      Nothing -> panic "Impossible!"
      Just v -> v

    -- `singular` turns a traversal into an affinetraversal, traversing just the
    -- first element, if present. It's an affine-traversal because we can't
    -- guarantee the traveral is non-empty.
    -- But in our case we do know this, because we always have the global scope.
    -- So it's fine to use.
    setter :: GroupScopes -> Scope -> GroupScopes
    setter c newScope = c & O.singular scopesTraversal !~ newScope

localProperty :: Lens' Scope (Maybe a) -> GroupScopes -> Maybe a
localProperty lens_ = scopedLookup (view lens_)

localCompleteProperty :: Lens' Scope (Maybe a) -> GroupScopes -> a
localCompleteProperty lens_ =
  fromMaybe (panic "No value found for supposedly complete property") . localProperty lens_

-- | Set a scoped property which is contained in a map, indexed by some key.
setScopedMapValue :: Ord k => (Lens' Scope (Map k v)) -> k -> v -> HSt.Grouped.ScopeFlag -> GroupScopes -> GroupScopes
setScopedMapValue mapLens c = setScopedProperty (mapLens % O.at' c)

-- | From a lens from a scope to a property, which might be empty, set that
-- value. (Actually we could weaken 'Lens' to 'Setter', but that would require a
-- bunch of `castOptic` calls.)
setScopedProperty :: (Lens' Scope (Maybe a)) -> a -> HSt.Grouped.ScopeFlag -> GroupScopes -> GroupScopes
setScopedProperty scopeValueLens newValue scopeFlag groupScopes =
  case scopeFlag of
    -- If doing a global assignment.
    -- - Set the global-scope value to 'Just' the new value.
    -- - Set all local-scope values to Nothing.
    HSt.Grouped.Global ->
      groupScopes
        & #globalScope % scopeValueLens ?!~ newValue
        & localScopesTraversal % scopeValueLens !~ Nothing
    -- If doing a local assignment,
    -- set the localmost scope's value to the new value.
    HSt.Grouped.Local ->
      groupScopes & localMostScopeLens % scopeValueLens ?!~ newValue

-- | Unset a scoped property which is contained in a map, indexed by some key.
unsetScopedMapValue :: Ord k => (Lens' Scope (Map k v)) -> k -> HSt.Grouped.ScopeFlag -> GroupScopes -> GroupScopes
unsetScopedMapValue mapLens c = unsetScopedProperty (mapLens % O.at' c)

-- | From a lens from a scope to a property, which might be empty, unset that
-- value.
unsetScopedProperty :: (Lens' Scope (Maybe a)) -> HSt.Grouped.ScopeFlag -> GroupScopes -> GroupScopes
unsetScopedProperty scopeValueLens scopeFlag groupScopes =
  -- If 'global', unset all scopes. If 'local', just unset the localmost scope.
  let scopesToUnsetTraversal = case scopeFlag of
        HSt.Grouped.Global -> scopesTraversal
        HSt.Grouped.Local -> castOptic localMostScopeLens
   in groupScopes & scopesToUnsetTraversal % scopeValueLens .~ Nothing
