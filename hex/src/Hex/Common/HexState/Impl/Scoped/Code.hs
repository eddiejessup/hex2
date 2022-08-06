module Hex.Common.HexState.Impl.Scoped.Code where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, localCompleteProperty, scopedMapValueLens)
import Hex.Common.HexState.Impl.Scoped.Scope (Scope (..))
import Hex.Common.HexState.Interface.Code qualified as HSt.Code
import Hexlude

hexCodeLens :: Code.CCodeType c -> Code.CharCode -> Lens' Scope (Maybe (HSt.Code.CodeTableTarget c))
hexCodeLens = \case
  Code.CCatCodeType -> scopedMapValueLens #catCodes
  Code.CMathCodeType -> scopedMapValueLens #mathCodes
  Code.CUpperCaseCodeType -> scopedMapValueLens #upperCaseCodes
  Code.CLowerCaseCodeType -> scopedMapValueLens #lowerCaseCodes
  Code.CSpaceFactorCodeType -> scopedMapValueLens #spaceFactorCodes
  Code.CDelimiterCodeType -> scopedMapValueLens #delimiterCodes

localHexCode :: Code.CCodeType c -> Code.CharCode -> GroupScopes -> HSt.Code.CodeTableTarget c
localHexCode ccodeType p = case ccodeType of
  Code.CCatCodeType -> localCompleteProperty (#catCodes % at' p)
  Code.CMathCodeType -> localCompleteProperty (#mathCodes % at' p)
  Code.CUpperCaseCodeType -> localCompleteProperty (#upperCaseCodes % at' p)
  Code.CLowerCaseCodeType -> localCompleteProperty (#lowerCaseCodes % at' p)
  Code.CSpaceFactorCodeType -> localCompleteProperty (#spaceFactorCodes % at' p)
  Code.CDelimiterCodeType -> localCompleteProperty (#delimiterCodes % at' p)
