module Hex.Common.HexState.Impl.Scoped.Font where

import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, localCompleteProperty, setScopedMapValue, setScopedProperty)
import Hex.Common.HexState.Impl.Scoped.Scope (Scope (..))
import Hex.Common.HexState.Interface.Font qualified as Font
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hex.Stage.Render.Interface.DocInstruction qualified as DVI
import Hexlude

localCurrentFontNr :: GroupScopes -> DVI.FontNumber
localCurrentFontNr = localCompleteProperty #currentFontNr

setCurrentFontNr :: DVI.FontNumber -> HSt.Grouped.ScopeFlag -> GroupScopes -> GroupScopes
setCurrentFontNr = setScopedProperty (castOptic #currentFontNr)

setFamilyMemberFont :: Font.FamilyMember -> DVI.FontNumber -> HSt.Grouped.ScopeFlag -> GroupScopes -> GroupScopes
setFamilyMemberFont = setScopedMapValue #familyMemberFonts
