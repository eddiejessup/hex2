module Hex.Common.HexState.Impl.Scoped.Parameter where

import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, localCompleteProperty, setScopedMapValue)
import Hex.Common.HexState.Impl.Scoped.Scope (Scope)
import Hex.Common.HexState.Impl.Scoped.Scope qualified as Scope
import Hex.Common.HexState.Interface.Parameter qualified as Param
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.HexState.Interface.Variable qualified as Var
import Hex.Common.Quantity qualified as Q
import Hexlude

class (Ord p, Q.Scalable (ScopedHexParameterValue p)) => ScopedHexParameter p where
  type ScopedHexParameterValue p

  scopeParameterMapLens :: Lens' Scope (Map p (ScopedHexParameterValue p))

instance ScopedHexParameter Param.IntParameter where
  type ScopedHexParameterValue Param.IntParameter = Q.HexInt

  scopeParameterMapLens = #intParameters

instance ScopedHexParameter Param.LengthParameter where
  type ScopedHexParameterValue Param.LengthParameter = Q.Length

  scopeParameterMapLens = #lengthParameters

instance ScopedHexParameter Param.GlueParameter where
  type ScopedHexParameterValue Param.GlueParameter = Q.Glue

  scopeParameterMapLens = #glueParameters

instance ScopedHexParameter Param.MathGlueParameter where
  type ScopedHexParameterValue Param.MathGlueParameter = Q.MathGlue

  scopeParameterMapLens = #mathGlueParameters

setParameterValue :: Param.QuantParam q -> Var.QuantVariableTarget q -> PT.ScopeFlag -> GroupScopes -> GroupScopes
setParameterValue = \case
  Param.IntQuantParam p -> setScopedMapValue scopeParameterMapLens p
  Param.LengthQuantParam p -> setScopedMapValue scopeParameterMapLens p
  Param.GlueQuantParam p -> setScopedMapValue scopeParameterMapLens p
  Param.MathGlueQuantParam p -> setScopedMapValue scopeParameterMapLens p
  Param.TokenListQuantParam _p -> notImplemented "setParameterValue: TokenListQuantParam"

localParameterValue :: Param.QuantParam q -> GroupScopes -> Var.QuantVariableTarget q
localParameterValue = \case
  Param.IntQuantParam p -> localCompleteProperty (scopeParameterMapLens % at' p)
  Param.LengthQuantParam p -> localCompleteProperty (scopeParameterMapLens % at' p)
  Param.GlueQuantParam p -> localCompleteProperty (scopeParameterMapLens % at' p)
  Param.MathGlueQuantParam p -> localCompleteProperty (scopeParameterMapLens % at' p)
  Param.TokenListQuantParam _p -> notImplemented "localCompleteProperty: TokenListQuantParam"
