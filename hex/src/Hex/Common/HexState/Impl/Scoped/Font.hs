module Hex.Common.HexState.Impl.Scoped.Font where

import Hex.Common.Font qualified as Font
import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, localCompleteProperty, scopedMapValueLens)
import Hex.Common.HexState.Impl.Scoped.Scope (Scope (..))
import Hex.Common.HexState.Interface.Font qualified as Font
import Hexlude

localCurrentFontNr :: GroupScopes -> Font.FontNumber
localCurrentFontNr = localCompleteProperty #currentFontNr

currentFontNrLens :: Lens' Scope (Maybe Font.FontNumber)
currentFontNrLens = #currentFontNr

familyMemberFontLens :: Font.FamilyMember -> Lens' Scope (Maybe Font.FontNumber)
familyMemberFontLens = scopedMapValueLens #familyMemberFonts
