module Hex.Common.HexState.Impl.Scoped.Code where

import Hex.Common.Codes qualified as Code
import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, localCompleteProperty, setScopedMapValue)
import Hex.Common.HexState.Impl.Scoped.Scope (Scope (..))
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hexlude

class Code.HexCode c => MutableHexCode c where
  scopeCodeMapLens :: Lens' Scope (Map Code.CharCode c)

instance MutableHexCode Code.CatCode where
  scopeCodeMapLens :: Lens' Scope (Map Code.CharCode Code.CatCode)
  scopeCodeMapLens = #catCodes

instance MutableHexCode Code.MathCode where
  scopeCodeMapLens :: Lens' Scope (Map Code.CharCode Code.MathCode)
  scopeCodeMapLens = #mathCodes

instance MutableHexCode Code.UpperCaseCode where
  scopeCodeMapLens :: Lens' Scope (Map Code.CharCode Code.UpperCaseCode)
  scopeCodeMapLens = #upperCaseCodes

instance MutableHexCode Code.LowerCaseCode where
  scopeCodeMapLens :: Lens' Scope (Map Code.CharCode Code.LowerCaseCode)
  scopeCodeMapLens = #lowerCaseCodes

instance MutableHexCode Code.SpaceFactorCode where
  scopeCodeMapLens :: Lens' Scope (Map Code.CharCode Code.SpaceFactorCode)
  scopeCodeMapLens = #spaceFactorCodes

instance MutableHexCode Code.DelimiterCode where
  scopeCodeMapLens :: Lens' Scope (Map Code.CharCode Code.DelimiterCode)
  scopeCodeMapLens = #delimiterCodes

setHexCode :: MutableHexCode c => Code.CharCode -> c -> PT.ScopeFlag -> GroupScopes -> GroupScopes
setHexCode = setScopedMapValue scopeCodeMapLens

localHexCode :: MutableHexCode c => Code.CharCode -> GroupScopes -> c
localHexCode p = localCompleteProperty (scopeCodeMapLens % at' p)
