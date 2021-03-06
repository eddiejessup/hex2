module Hex.Common.HexState.Impl.Scoped.Register where

import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes)
import Hex.Common.HexState.Impl.Scoped.GroupScopes qualified as HSt.GroupScopes
import Hex.Common.HexState.Impl.Scoped.Scope qualified as Scope
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hex.Common.HexState.Interface.Register qualified as HSt.Reg
import Hex.Common.HexState.Interface.TokenList qualified as HSt.TL
import Hex.Common.HexState.Interface.Variable qualified as HSt.Var
import Hex.Common.Quantity qualified as Q
import Hex.Stage.Build.BoxElem qualified as Box
import Hexlude

setQuantRegisterValue :: HSt.Reg.QuantRegisterLocation q -> HSt.Var.QuantVariableTarget q -> HSt.Grouped.ScopeFlag -> GroupScopes -> GroupScopes
setQuantRegisterValue (HSt.Reg.QuantRegisterLocation regType loc) = case regType of
  HSt.Reg.IntQuantRegisterType -> HSt.GroupScopes.setScopedMapValue #intRegister loc
  HSt.Reg.LengthQuantRegisterType -> HSt.GroupScopes.setScopedMapValue #lengthRegister loc
  HSt.Reg.GlueQuantRegisterType -> HSt.GroupScopes.setScopedMapValue #glueRegister loc
  HSt.Reg.MathGlueQuantRegisterType -> HSt.GroupScopes.setScopedMapValue #mathGlueRegister loc
  HSt.Reg.TokenListQuantRegisterType -> HSt.GroupScopes.setScopedMapValue #tokenListRegister loc

localQuantRegisterValue :: HSt.Reg.QuantRegisterLocation q -> GroupScopes -> HSt.Var.QuantVariableTarget q
localQuantRegisterValue (HSt.Reg.QuantRegisterLocation regType loc) = case regType of
  HSt.Reg.IntQuantRegisterType -> fromMaybe Q.zeroInt . HSt.GroupScopes.localProperty (#intRegister % at' loc)
  HSt.Reg.LengthQuantRegisterType -> fromMaybe Q.zeroLength . HSt.GroupScopes.localProperty (#lengthRegister % at' loc)
  HSt.Reg.GlueQuantRegisterType -> fromMaybe Q.zeroGlue . HSt.GroupScopes.localProperty (#glueRegister % at' loc)
  HSt.Reg.MathGlueQuantRegisterType -> fromMaybe Q.zeroMathGlue . HSt.GroupScopes.localProperty (#mathGlueRegister % at' loc)
  HSt.Reg.TokenListQuantRegisterType -> fromMaybe HSt.TL.emptyBalancedText . HSt.GroupScopes.localProperty (#tokenListRegister % at' loc)

setBoxRegisterValue :: HSt.Reg.RegisterLocation -> Box.BaseBox -> HSt.Grouped.ScopeFlag -> GroupScopes -> GroupScopes
setBoxRegisterValue = HSt.GroupScopes.setScopedMapValue #boxRegister

unsetBoxRegisterValue :: HSt.Reg.RegisterLocation -> HSt.Grouped.ScopeFlag -> GroupScopes -> GroupScopes
unsetBoxRegisterValue = HSt.GroupScopes.unsetScopedMapValue #boxRegister

localBoxRegisterValue :: HSt.Reg.RegisterLocation -> GroupScopes -> Maybe Box.BaseBox
localBoxRegisterValue loc = HSt.GroupScopes.localProperty (#boxRegister % at' loc)
