module Hex.Common.HexState.Impl.Scoped.Symbol where

import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, localProperty, scopedMapValueLens)
import Hex.Common.HexState.Impl.Scoped.Scope (Scope (..))
import Hex.Common.HexState.Interface.Resolve (ControlSymbol)
import Hex.Common.Token.Resolved (ResolvedToken)
import Hexlude

localResolvedToken :: ControlSymbol -> GroupScopes -> Maybe ResolvedToken
localResolvedToken p =
  localProperty (#symbolMap % at' p)

symbolLens :: ControlSymbol -> Lens' Scope (Maybe ResolvedToken)
symbolLens = scopedMapValueLens #symbolMap
