module Hex.Common.HexState.Impl.Scoped.Parameter where

import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, localCompleteProperty, setScopedMapValue)
import Hex.Common.HexState.Impl.Scoped.Scope qualified as Scope
import Hex.Common.HexState.Interface.Grouped qualified as HSt.Grouped
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.HexState.Interface.Variable qualified as HSt.Var
import Hexlude

setParameterValue :: HSt.Param.QuantParam q -> HSt.Var.QuantVariableTarget q -> HSt.Grouped.ScopeFlag -> GroupScopes -> GroupScopes
setParameterValue = \case
  HSt.Param.IntQuantParam p -> setScopedMapValue #intParameters p
  HSt.Param.LengthQuantParam p -> setScopedMapValue #lengthParameters p
  HSt.Param.GlueQuantParam p -> setScopedMapValue #glueParameters p
  HSt.Param.MathGlueQuantParam p -> setScopedMapValue #mathGlueParameters p
  HSt.Param.TokenListQuantParam p -> setScopedMapValue #tokenListParameters p

localParameterValue :: HSt.Param.QuantParam q -> GroupScopes -> HSt.Var.QuantVariableTarget q
localParameterValue = \case
  HSt.Param.IntQuantParam p -> localCompleteProperty (#intParameters % at' p)
  HSt.Param.LengthQuantParam p -> localCompleteProperty (#lengthParameters % at' p)
  HSt.Param.GlueQuantParam p -> localCompleteProperty (#glueParameters % at' p)
  HSt.Param.MathGlueQuantParam p -> localCompleteProperty (#mathGlueParameters % at' p)
  HSt.Param.TokenListQuantParam p -> localCompleteProperty (#tokenListParameters % at' p)
