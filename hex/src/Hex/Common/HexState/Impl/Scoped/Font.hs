module Hex.Common.HexState.Impl.Scoped.Font where

import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, setScopedProperty, localCompleteProperty)
import Hex.Common.HexState.Impl.Scoped.Scope (Scope (..))
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hexlude

localCurrentFontNr :: GroupScopes -> PT.FontNumber
localCurrentFontNr = localCompleteProperty #currentFontNr

setCurrentFontNr :: PT.FontNumber -> PT.ScopeFlag -> GroupScopes -> GroupScopes
setCurrentFontNr = setScopedProperty (castOptic #currentFontNr)
