module Hex.Common.Quantity.Common where

import Hexlude

data QuantityType
  = IntQuantity -- \count, \countdef
  | LengthQuantity -- \dimen, \dimendef
  | GlueQuantity -- \skip, \skipdef
  | MathGlueQuantity -- \muskip, \muskipdef
  | TokenListQuantity -- \toks, \toksdef
  deriving stock (Show, Eq, Generic)
