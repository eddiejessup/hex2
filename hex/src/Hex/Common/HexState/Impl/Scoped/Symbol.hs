module Hex.Common.HexState.Impl.Scoped.Symbol where

import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, localProperty, setScopedMapValue)
import Hex.Common.HexState.Impl.Scoped.Scope (Scope (..))
import Hex.Common.HexState.Interface.Resolve (ControlSymbol, ResolvedToken)
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hexlude

localResolvedToken :: ControlSymbol -> GroupScopes -> Maybe ResolvedToken
localResolvedToken p =
  localProperty (#symbolMap % at' p)

setSymbol :: ControlSymbol -> ResolvedToken -> PT.ScopeFlag -> GroupScopes -> GroupScopes
setSymbol = setScopedMapValue #symbolMap
