module Hex.Common.HexState.Impl.GroupScopes where

import Hex.Common.Codes qualified as Codes
import Hex.Common.HexState.Impl.Group
import Hex.Common.HexState.Impl.Scope (Scope, newGlobalScope)
import Hex.Common.HexState.Impl.Scope qualified as Scope
import Hex.Common.HexState.Interface.Resolve (ControlSymbol, ResolvedToken)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Quantity qualified as H.Q
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

newGroupScopes :: GroupScopes
newGroupScopes = GroupScopes {globalScope = newGlobalScope, groups = []}

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
    -- Traversal of the localmost scope.
    -- In reality this is a lens, but I can't pro

    getter :: GroupScopes -> Scope
    getter grpScopes = case headOf localScopesTraversal grpScopes of
      Nothing -> panic "Impossible!"
      Just v -> v

    -- `singular` turns a traversal into an affinetraversal, traversing just the
    -- first element, if present. It's an affine-traversal because we can't
    -- guarantee the traveral is non-empty.
    -- But in our case we do know this, because we always have the global scope.
    -- So it's fine to use.
    setter :: GroupScopes -> Scope -> GroupScopes
    setter c newScope = c & O.singular scopesTraversal !~ newScope

localResolvedToken :: ControlSymbol -> GroupScopes -> Maybe ResolvedToken
localResolvedToken p =
  scopedLookup
    (view (Scope.scopeResolvedTokenLens p))

localCategory :: Codes.CharCode -> GroupScopes -> Codes.CatCode
localCategory p =
  fromMaybe Codes.Invalid
    . scopedLookup
      (view (Scope.scopeCategoryLens p))

localIntParam :: PT.IntParameter -> GroupScopes -> H.Q.HexInt
localIntParam p =
  fromMaybe H.Q.zeroInt
    . scopedLookup
      (view (Scope.scopeIntParamLens p))

localLengthParam :: PT.LengthParameter -> GroupScopes -> H.Q.Length
localLengthParam p =
  fromMaybe H.Q.zeroLength
    . scopedLookup
      (view (Scope.scopeLengthParamLens p))

localGlueParam :: PT.GlueParameter -> GroupScopes -> H.Q.Glue
localGlueParam p =
  fromMaybe H.Q.zeroGlue
    . scopedLookup (view (Scope.scopeGlueParamLens p))

localCurrentFontNr :: GroupScopes -> Maybe PT.FontNumber
localCurrentFontNr = scopedLookup (view #currentFontNr)

setCurrentFontNr :: PT.FontNumber -> PT.ScopeFlag -> GroupScopes -> GroupScopes
setCurrentFontNr = setScopedProperty (castOptic #currentFontNr)

setScopedProperty :: (Setter' Scope (Maybe a)) -> a -> PT.ScopeFlag -> GroupScopes -> GroupScopes
setScopedProperty scopeValueLens newValue scopeFlag groupScopes =
  case scopeFlag of
    -- If doing a global assignment.
    -- - Set the global-scope value to 'Just' the new value.
    -- - Set all local-scope values to Nothing.
    PT.Global ->
      groupScopes
        & #globalScope % scopeValueLens ?!~ newValue
        & localScopesTraversal % scopeValueLens !~ Nothing
    -- If doing a local assignment,
    -- set the localmost scope's value to the new value.
    PT.Local ->
      groupScopes & localMostScopeLens % scopeValueLens ?!~ newValue
