module Hex.Quantity.MathGlue where

import Hex.Quantity.Glue
import Hex.Quantity.MathLength
import Hexlude

data MathGlue = MathGlue {mgDimen :: MathLength, mgStretch :: PureFlex, mgShrink :: PureFlex}
  deriving stock (Show, Generic)
