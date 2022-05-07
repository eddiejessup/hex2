module Hex.Common.HexState.Impl.Scoped.Font where

import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, localProperty, setScopedProperty)
import Hex.Common.HexState.Impl.Scoped.Scope (Scope (..))
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hexlude

localCurrentFontNr :: GroupScopes -> Maybe PT.FontNumber
localCurrentFontNr = localProperty #currentFontNr

setCurrentFontNr :: PT.FontNumber -> PT.ScopeFlag -> GroupScopes -> GroupScopes
setCurrentFontNr = setScopedProperty (castOptic #currentFontNr)
