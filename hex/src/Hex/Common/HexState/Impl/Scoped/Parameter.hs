module Hex.Common.HexState.Impl.Scoped.Parameter where

import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, localCompleteProperty, setScopedMapValue)
import Hex.Common.HexState.Impl.Scoped.Scope qualified as Scope
import Hex.Common.HexState.Interface.Parameter qualified as Param
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Variable qualified as Var
import Hexlude

setParameterValue :: Param.QuantParam q -> Var.QuantVariableTarget q -> PT.ScopeFlag -> GroupScopes -> GroupScopes
setParameterValue = \case
  Param.IntQuantParam p -> setScopedMapValue #intParameters p
  Param.LengthQuantParam p -> setScopedMapValue #lengthParameters p
  Param.GlueQuantParam p -> setScopedMapValue #glueParameters p
  Param.MathGlueQuantParam p -> setScopedMapValue #mathGlueParameters p
  Param.TokenListQuantParam _p -> notImplemented "setParameterValue: TokenListQuantParam"

localParameterValue :: Param.QuantParam q -> GroupScopes -> Var.QuantVariableTarget q
localParameterValue = \case
  Param.IntQuantParam p -> localCompleteProperty (#intParameters % at' p)
  Param.LengthQuantParam p -> localCompleteProperty (#lengthParameters % at' p)
  Param.GlueQuantParam p -> localCompleteProperty (#glueParameters % at' p)
  Param.MathGlueQuantParam p -> localCompleteProperty (#mathGlueParameters % at' p)
  Param.TokenListQuantParam _p -> notImplemented "localCompleteProperty: TokenListQuantParam"
