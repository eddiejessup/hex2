module Hex.Common.HexState.Impl.Scoped.Font where

import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, localCompleteProperty, setScopedMapValue, setScopedProperty)
import Hex.Common.HexState.Impl.Scoped.Scope (Scope (..))
import Hex.Common.HexState.Interface.Font qualified as Font
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hexlude

localCurrentFontNr :: GroupScopes -> Font.FontNumber
localCurrentFontNr = localCompleteProperty #currentFontNr

setCurrentFontNr :: Font.FontNumber -> HSt.Grouped.ScopeFlag -> GroupScopes -> GroupScopes
setCurrentFontNr = setScopedProperty (castOptic #currentFontNr)

setFamilyMemberFont :: Font.FamilyMember -> Font.FontNumber -> HSt.Grouped.ScopeFlag -> GroupScopes -> GroupScopes
setFamilyMemberFont = setScopedMapValue #familyMemberFonts
