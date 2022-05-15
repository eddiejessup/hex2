module Hex.Common.HexState.Impl.Scoped.Register where

import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, localProperty, setScopedMapValue)
import Hex.Common.HexState.Impl.Scoped.Scope qualified as Scope
import Hex.Common.HexState.Interface.Register qualified as Reg
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Variable qualified as Var
import Hex.Common.Quantity qualified as Q
import Hexlude

setRegisterValue :: Reg.QuantRegisterLocation q -> (Var.QuantVariableTarget q) -> PT.ScopeFlag -> GroupScopes -> GroupScopes
setRegisterValue (Reg.QuantRegisterLocation regType loc) = case regType of
  Reg.IntQuantRegisterType -> setScopedMapValue #intRegister loc
  Reg.LengthQuantRegisterType -> setScopedMapValue #lengthRegister loc
  Reg.GlueQuantRegisterType -> setScopedMapValue #glueRegister loc
  Reg.MathGlueQuantRegisterType -> setScopedMapValue #mathGlueRegister loc
  Reg.TokenListQuantRegisterType -> notImplemented "setRegisterValue: TokenListQuantRegisterType"

localRegisterValue :: Reg.QuantRegisterLocation q -> GroupScopes -> Var.QuantVariableTarget q
localRegisterValue (Reg.QuantRegisterLocation regType loc) = case regType of
  Reg.IntQuantRegisterType -> fromMaybe Q.zeroInt . localProperty (#intRegister % at' loc)
  Reg.LengthQuantRegisterType -> fromMaybe Q.zeroLength . localProperty (#lengthRegister % at' loc)
  Reg.GlueQuantRegisterType -> fromMaybe Q.zeroGlue . localProperty (#glueRegister % at' loc)
  Reg.MathGlueQuantRegisterType -> fromMaybe Q.zeroMathGlue . localProperty (#mathGlueRegister % at' loc)
  Reg.TokenListQuantRegisterType -> notImplemented "setRegisterValue: TokenListQuantRegisterType"
