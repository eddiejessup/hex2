module Hex.Common.HexState.Impl.Scoped.Parameter where

import Hex.Common.HexState.Impl.Scoped.GroupScopes (GroupScopes, localCompleteProperty, setScopedMapValue)
import Hex.Common.HexState.Impl.Scoped.Scope (Scope)
import Hex.Common.HexState.Impl.Scoped.Scope qualified as Scope
import Hex.Common.HexState.Interface.Resolve.PrimitiveToken qualified as PT
import Hex.Common.Quantity qualified as Q
import Hexlude

class (Ord p, Q.Scalable (ScopedHexParameterValue p)) => ScopedHexParameter p where
  type ScopedHexParameterValue p

  scopeParameterMapLens :: Lens' Scope (Map p (ScopedHexParameterValue p))

instance ScopedHexParameter PT.IntParameter where
  type ScopedHexParameterValue PT.IntParameter = Q.HexInt

  scopeParameterMapLens = #intParameters

instance ScopedHexParameter PT.LengthParameter where
  type ScopedHexParameterValue PT.LengthParameter = Q.Length

  scopeParameterMapLens = #lengthParameters

instance ScopedHexParameter PT.GlueParameter where
  type ScopedHexParameterValue PT.GlueParameter = Q.Glue

  scopeParameterMapLens = #glueParameters

instance ScopedHexParameter PT.MathGlueParameter where
  type ScopedHexParameterValue PT.MathGlueParameter = Q.MathGlue

  scopeParameterMapLens = #mathGlueParameters

setParameterValue :: ScopedHexParameter p => p -> (ScopedHexParameterValue p) -> PT.ScopeFlag -> GroupScopes -> GroupScopes
setParameterValue = setScopedMapValue scopeParameterMapLens

localParameterValue :: ScopedHexParameter p => p -> GroupScopes -> (ScopedHexParameterValue p)
localParameterValue p = localCompleteProperty (scopeParameterMapLens % at' p)
