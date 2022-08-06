module Hex.Common.HexState.Impl.Scoped.Font where

import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, localCompleteProperty, scopedMapValueLens)
import Hex.Common.HexState.Impl.Scoped.Scope (Scope (..))
import Hex.Common.HexState.Interface.Font qualified as Font
import Hex.Stage.Render.Interface.DocInstruction qualified as DVI
import Hexlude

localCurrentFontNr :: GroupScopes -> DVI.FontNumber
localCurrentFontNr = localCompleteProperty #currentFontNr

currentFontNrLens :: Lens' Scope (Maybe DVI.FontNumber)
currentFontNrLens = #currentFontNr

familyMemberFontLens :: Font.FamilyMember -> Lens' Scope (Maybe DVI.FontNumber)
familyMemberFontLens = scopedMapValueLens #familyMemberFonts
