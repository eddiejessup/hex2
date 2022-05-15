{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Hex.Common.HexState.Interface.Variable where

import Hex.Common.HexState.Interface.Parameter qualified as HSt.Param
import Hex.Common.HexState.Interface.Register qualified as HSt.Reg
import Hex.Common.HexState.Interface.TokenList qualified as TL
import Hex.Common.Quantity qualified as Q
import Hexlude

type family QuantVariableTarget (q :: Q.QuantityType) = result | result -> q where
  QuantVariableTarget 'Q.IntQuantity = Q.HexInt
  QuantVariableTarget 'Q.LengthQuantity = Q.Length
  QuantVariableTarget 'Q.GlueQuantity = Q.Glue
  QuantVariableTarget 'Q.MathGlueQuantity = Q.MathGlue
  QuantVariableTarget 'Q.TokenListQuantity = TL.BalancedText

data QuantVariable (q :: Q.QuantityType)
  = ParamVar (HSt.Param.QuantParam q)
  | RegisterVar (HSt.Reg.QuantRegisterLocation q)
  deriving stock (Generic)

deriving stock instance Show (HSt.Param.QuantParam q) => Show (QuantVariable q)

deriving stock instance Eq (HSt.Param.QuantParam q) => Eq (QuantVariable q)
