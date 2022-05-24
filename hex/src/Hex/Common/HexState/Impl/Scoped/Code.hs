module Hex.Common.HexState.Impl.Scoped.Code where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, localCompleteProperty, setScopedMapValue)
import Hex.Common.HexState.Impl.Scoped.Scope (Scope (..))
import Hex.Common.HexState.Interface.Code qualified as HSt.Code
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hexlude

setHexCode :: Code.CCodeType c -> Code.CharCode -> HSt.Code.CodeTableTarget c -> HSt.Grouped.ScopeFlag -> GroupScopes -> GroupScopes
setHexCode = \case
  Code.CCatCodeType -> setScopedMapValue #catCodes
  Code.CMathCodeType -> setScopedMapValue #mathCodes
  Code.CUpperCaseCodeType -> setScopedMapValue #upperCaseCodes
  Code.CLowerCaseCodeType -> setScopedMapValue #lowerCaseCodes
  Code.CSpaceFactorCodeType -> setScopedMapValue #spaceFactorCodes
  Code.CDelimiterCodeType -> setScopedMapValue #delimiterCodes

localHexCode :: Code.CCodeType c -> Code.CharCode -> GroupScopes -> HSt.Code.CodeTableTarget c
localHexCode ccodeType p = case ccodeType of
  Code.CCatCodeType -> localCompleteProperty (#catCodes % at' p)
  Code.CMathCodeType -> localCompleteProperty (#mathCodes % at' p)
  Code.CUpperCaseCodeType -> localCompleteProperty (#upperCaseCodes % at' p)
  Code.CLowerCaseCodeType -> localCompleteProperty (#lowerCaseCodes % at' p)
  Code.CSpaceFactorCodeType -> localCompleteProperty (#spaceFactorCodes % at' p)
  Code.CDelimiterCodeType -> localCompleteProperty (#delimiterCodes % at' p)
