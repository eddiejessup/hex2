module Hex.Common.HexState.Impl.Scoped.Register where

import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, localProperty, setScopedMapValue)
import Hex.Common.HexState.Impl.Scoped.Scope (Scope)
import Hex.Common.HexState.Impl.Scoped.Scope qualified as Scope
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Quantity qualified as Q
import Hexlude

class Q.Scalable r => ScopedHexRegisterValue r where
  scopeRegisterMapLens :: Lens' Scope (Map Scope.RegisterLocation r)

  registerDefault :: r

instance ScopedHexRegisterValue Q.HexInt where
  scopeRegisterMapLens = #intRegister

  registerDefault = Q.zeroInt

instance ScopedHexRegisterValue Q.Length where
  scopeRegisterMapLens = #lengthRegister

  registerDefault = Q.zeroLength

instance ScopedHexRegisterValue Q.Glue where
  scopeRegisterMapLens = #glueRegister

  registerDefault = Q.zeroGlue

instance ScopedHexRegisterValue Q.MathGlue where
  scopeRegisterMapLens = #mathGlueRegister

  registerDefault = Q.zeroMathGlue

setRegisterValue :: ScopedHexRegisterValue r => Scope.RegisterLocation -> r -> PT.ScopeFlag -> GroupScopes -> GroupScopes
setRegisterValue = setScopedMapValue scopeRegisterMapLens

localRegisterValue :: ScopedHexRegisterValue r => Scope.RegisterLocation -> GroupScopes -> r
localRegisterValue r = fromMaybe registerDefault . localProperty (scopeRegisterMapLens % at' r)
