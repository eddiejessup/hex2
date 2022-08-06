module Hex.Common.HexState.Impl.Scoped.Parameter where

import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, localCompleteProperty, scopedMapValueLens)
import Hex.Common.HexState.Impl.Scoped.Scope qualified as Scope
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.HexState.Interface.Variable qualified as HSt.Var
import Hexlude

parameterValueLens :: HSt.Param.QuantParam q -> Lens' Scope.Scope (Maybe (HSt.Var.QuantVariableTarget q))
parameterValueLens = \case
  HSt.Param.IntQuantParam p -> scopedMapValueLens #intParameters p
  HSt.Param.LengthQuantParam p -> scopedMapValueLens #lengthParameters p
  HSt.Param.GlueQuantParam p -> scopedMapValueLens #glueParameters p
  HSt.Param.MathGlueQuantParam p -> scopedMapValueLens #mathGlueParameters p
  HSt.Param.TokenListQuantParam p -> scopedMapValueLens #tokenListParameters p

localParameterValue :: HSt.Param.QuantParam q -> GroupScopes -> HSt.Var.QuantVariableTarget q
localParameterValue = \case
  HSt.Param.IntQuantParam p -> localCompleteProperty (#intParameters % at' p)
  HSt.Param.LengthQuantParam p -> localCompleteProperty (#lengthParameters % at' p)
  HSt.Param.GlueQuantParam p -> localCompleteProperty (#glueParameters % at' p)
  HSt.Param.MathGlueQuantParam p -> localCompleteProperty (#mathGlueParameters % at' p)
  HSt.Param.TokenListQuantParam p -> localCompleteProperty (#tokenListParameters % at' p)
