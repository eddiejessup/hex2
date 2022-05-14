{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.HexState.Interface.Variable where

import Hex.Common.HexState.Impl.Scoped.Scope qualified as Scope
import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.HexState.Interface.TokenList qualified as TL
import Hex.Common.Quantity qualified as Q
import Hexlude

type family QuantVariableTarget (a :: Q.QuantityType) = result | result -> a where
  QuantVariableTarget 'Q.IntQuantity = Q.HexInt
  QuantVariableTarget 'Q.LengthQuantity = Q.Length
  QuantVariableTarget 'Q.GlueQuantity = Q.Glue
  QuantVariableTarget 'Q.MathGlueQuantity = Q.MathGlue
  QuantVariableTarget 'Q.TokenListQuantity = TL.BalancedText

data QuantVariable (a :: Q.QuantityType)
  = ParamVar (HSt.Param.QuantParam a)
  | RegisterVar Scope.RegisterLocation
  deriving stock (Generic)

deriving stock instance Show (HSt.Param.QuantParam a) => Show (QuantVariable a)

deriving stock instance Eq (HSt.Param.QuantParam a) => Eq (QuantVariable a)
