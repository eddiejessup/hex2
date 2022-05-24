module Hex.Common.HexState.Impl.Scoped.Symbol where

import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, localProperty, setScopedMapValue)
import Hex.Common.HexState.Impl.Scoped.Scope (Scope (..))
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hex.Common.HexState.Interface.Resolve (ControlSymbol, ResolvedToken)
import Hexlude

localResolvedToken :: ControlSymbol -> GroupScopes -> Maybe ResolvedToken
localResolvedToken p =
  localProperty (#symbolMap % at' p)

setSymbol :: ControlSymbol -> ResolvedToken -> HSt.Grouped.ScopeFlag -> GroupScopes -> GroupScopes
setSymbol = setScopedMapValue #symbolMap
