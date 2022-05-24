module Hex.Common.HexState.Impl.Scoped.Register where

import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, localProperty, setScopedMapValue)
import Hex.Common.HexState.Impl.Scoped.Scope qualified as Scope
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hex.Common.HexState.Interface.Register qualified as HSt.Reg
import Hex.Common.HexState.Interface.Variable qualified as HSt.Var
import Hex.Common.Quantity qualified as Q
import Hexlude

setRegisterValue :: HSt.Reg.QuantRegisterLocation q -> (HSt.Var.QuantVariableTarget q) -> HSt.Grouped.ScopeFlag -> GroupScopes -> GroupScopes
setRegisterValue (HSt.Reg.QuantRegisterLocation regType loc) = case regType of
  HSt.Reg.IntQuantRegisterType -> setScopedMapValue #intRegister loc
  HSt.Reg.LengthQuantRegisterType -> setScopedMapValue #lengthRegister loc
  HSt.Reg.GlueQuantRegisterType -> setScopedMapValue #glueRegister loc
  HSt.Reg.MathGlueQuantRegisterType -> setScopedMapValue #mathGlueRegister loc
  HSt.Reg.TokenListQuantRegisterType -> notImplemented "setRegisterValue: TokenListQuantRegisterType"

localRegisterValue :: HSt.Reg.QuantRegisterLocation q -> GroupScopes -> HSt.Var.QuantVariableTarget q
localRegisterValue (HSt.Reg.QuantRegisterLocation regType loc) = case regType of
  HSt.Reg.IntQuantRegisterType -> fromMaybe Q.zeroInt . localProperty (#intRegister % at' loc)
  HSt.Reg.LengthQuantRegisterType -> fromMaybe Q.zeroLength . localProperty (#lengthRegister % at' loc)
  HSt.Reg.GlueQuantRegisterType -> fromMaybe Q.zeroGlue . localProperty (#glueRegister % at' loc)
  HSt.Reg.MathGlueQuantRegisterType -> fromMaybe Q.zeroMathGlue . localProperty (#mathGlueRegister % at' loc)
  HSt.Reg.TokenListQuantRegisterType -> notImplemented "setRegisterValue: TokenListQuantRegisterType"
